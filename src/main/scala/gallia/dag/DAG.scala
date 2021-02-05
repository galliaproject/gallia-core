package gallia.dag

import aptus.{Anything_, Seq_}

import aptus.Nes
import aptus.Count

// ===========================================================================
class DAG[$NodeType](
        //TODO: change these to List
        val nodes     : Seq[$NodeType], // will always be relatively "small" (eg not millions of nodes)
        val edges     : Seq[Edge],      // will be pretty sparse so no need for density matrix
        val idResolver: $NodeType => NodeId // less painful than subtyping...
      ) {
    private type Self = DAG[$NodeType]

    // TODO: (t210116120349 - easier with graph lib?)
    // - enforce acyclism
    // - check no isolates - TODO: will create issues for chain-traversal... (see 201123165502)
    // - check nodes are DOT friendly (see t210114113316)

    private[dag] lazy val _lookup: Map[NodeId, $NodeType] = nodes.map(_.associateLeft(idResolver)).force.map

                 def lookup(nodeId: NodeId): $NodeType = _lookup(nodeId) // used in ASG creation
                 val nodeIds  : Seq[NodeId] = nodes.map(idResolver) // used in Handler's "join2"
    private[dag] val nodeIdSet: Set[NodeId] = nodeIds.toSet

    @deprecated("still needed?") def toIdDAG: IdDAG = new DAG[NodeId](nodeIds, edges, identity) // TODO: minimize use

    // ---------------------------------------------------------------------------
    require(
      (nodes.size == 0 && edges.size == 0) ||
      (nodes.size == 1 && edges.size == 0) ||
      (nodes.size >  1 && edges.size >  0),
      (nodes.size, edges.size, this)) // TODO: format dot

    require(
      nodeIds.isDistinct,
      nodeIds.duplicates.section2)

    require(
      edges.isDistinct,
      edges.duplicates.section2)

    require(
      edges.forall(x => x._1 != x._2),
      formatDot)

    // ===========================================================================
    def formatDefault = kahnTraverseNodes.map(_.toString).joinln

      // ---------------------------------------------------------------------------
      // TODO: t210114113316 - reenable graphviz dot's rendering
      def formatDot: String = ???//gallia.graphviz.DAGDot.formatIdDot(toIdDAG)
      def  printDot: Self   = { formatDot.p; this }

      // ---------------------------------------------------------------------------
      def formatDot(lookup: $NodeType => (String /* label */, String /* color */)): String = ???//gallia.graphviz.DAGDot.formatDot(this)(lookup)
      def  printDot(lookup: $NodeType => (String /* label */, String /* color */)): Self   = { formatDot(lookup).p; this }

      // ---------------------------------------------------------------------------
      private def defaultDotStyle(node: $NodeType) = (idResolver(node), "White")

    // ===========================================================================
    private[dag] lazy val _efferents: Map[NodeId, Nes[NodeId]] = edges.groupBy(_._1).mapValues(_.map(_._2))
    private[dag] lazy val _afferents: Map[NodeId, Nes[NodeId]] = edges.groupBy(_._2).mapValues(_.map(_._1))

    // ---------------------------------------------------------------------------
    lazy val roots : Seq[$NodeType] = nodeIds.diff(_afferents.keys.toSeq).map(_lookup)
    lazy val leaves: Seq[$NodeType] = nodeIds.diff(_efferents.keys.toSeq).map(_lookup)

    lazy val rootIds : Seq[NodeId] = roots .map(idResolver)
    lazy val leaveIds: Seq[NodeId] = leaves.map(idResolver)

    // ---------------------------------------------------------------------------
    def afferentIds(id: NodeId): Seq[NodeId] = _afferents.get(id).toSeq.flatten
    def efferentIds(id: NodeId): Seq[NodeId] = _efferents.get(id).toSeq.flatten

    def afferentCount(id: NodeId): Count = _afferents.get(id).map(_.size).getOrElse(0)
    def efferentCount(id: NodeId): Count = _efferents.get(id).map(_.size).getOrElse(0)

    def afferentNodes(id: NodeId): Seq[$NodeType] = afferentIds(id).map(_lookup)
    def efferentNodes(id: NodeId): Seq[$NodeType] = efferentIds(id).map(_lookup)

    // ---------------------------------------------------------------------------
    def  kahnTraverseIds: Seq[NodeId] = if (nodeIds.size <= 1) nodeIds else GraphUtils.kahn(edges.sorted).right.get.toList
    def chainTraverseIds: Seq[NodeId] = ChainUtils.chainTraversal(this.toIdDAG)(this.rootIds) // TODO: still needed?

    def  kahnTraverseNodes: Seq[$NodeType] = kahnTraverseIds.map(_lookup)
    def chainTraverseNodes: Seq[$NodeType] = ChainUtils.chainTraversal(this)(this.rootIds)

    // ---------------------------------------------------------------------------
    // TODO: id versions
    def trimRoots  = removeNodes( roots.toSet)
    def trimLeaves = removeNodes(leaves.toSet)

    // FIXME: assert root/leaf
    def trimRoot(root: $NodeType) = removeNodes(Set(root))
    def trimLeaf(leaf: $NodeType) = removeNodes(Set(leaf))

    // ---------------------------------------------------------------------------
    def isEmpty   : Boolean = nodes.isEmpty
    def isTrivial : Boolean = nodes.size == 1

     // TODO: optimize
    def isRoot(id: NodeId): Boolean = rootIds .exists(_ == id)
    def isLeaf(id: NodeId): Boolean = leaveIds.exists(_ == id)

    def hasOneRoot: Boolean = hasNRoots(n = 1)
    def hasOneLeaf: Boolean = hasNLeaves(n = 1)

    def isViolin           : Boolean = hasOneRoot && hasOneLeaf
    def isNonTrivialViolin : Boolean = !isTrivial && isViolin
    // TODO: hasDiamond (good for optimization, especially if none), isChain, ...

    def hasNRoots (n: Int): Boolean = rootIds .size == n
    def hasNLeaves(n: Int): Boolean = leaveIds.size == n

    // ===========================================================================
    def emptyToTrivial(root: $NodeType): Self = { require(isEmpty, this); DAG.trivial(idResolver)(root) }

    // ---------------------------------------------------------------------------
    def prependNode(pair: ($NodeType, NodeId)): Self = addNode(pair._1, idResolver(pair._1) -> pair._2)
    def  appendNode(pair: (NodeId, $NodeType)): Self = addNode(pair._2, pair._1 -> idResolver(pair._2))

    // ===========================================================================
    def afferentSubGraph(id: NodeId): Self = AfferentSubGraphExtractor(this)(id) // used by run

    // ---------------------------------------------------------------------------
    def pruneAsg(cached: NodeId => Boolean): Self = CachePruning(this, cached) // used by data run (caching); TODO: check asg? -- still needed?

    // ---------------------------------------------------------------------------
    def chain(start: NodeId): Chain[$NodeType] = ChainUtils.chain(this)(start, _lookup(start), skipAfferentCheck = true) // used by data run (chaining)

      // ---------------------------------------------------------------------------
      def isChain: Boolean =
        rootIds
          .headOption
          .forall { rootId =>
            chain(rootId).edges.sorted == this.edges.sorted }

      def forceChain = { assert(isChain, this); rootIds.force.one.thn(chain) }

    // ===========================================================================
    def transform[$NewNodeType]
            (f: $NodeType => $NewNodeType, g: NodeId => NodeId = identity)
            (newIdResolver: $NewNodeType => NodeId)
          : DAG[$NewNodeType] =
        new DAG[$NewNodeType](
          nodes
          .toList.map(f),
          edges.map(x => (g(x._1), g(x._2))), //TODO:?
          newIdResolver)

      // TODO: un-generalize...
      def transform2[A, B](f: A => B)(implicit ev: $NodeType <:< (NodeId, A)): DAG[(NodeId, B)] =
          transform({ case (id, x) => (id.asInstanceOf[NodeId], f(x.asInstanceOf[A])) })(_._1)

        def transform3[B](implicit ev: $NodeType <:< (NodeId, Any)): DAG[(NodeId, B)] = transform2((x: Any) => x.asInstanceOf[B])

        def transform4[B <: HasNodeId](f: $NodeType => B)  (implicit ev: $NodeType <:< HasNodeId): DAG[B]      = transform(f)(_.id)
        def transform5                (f: NodeId => NodeId)(implicit ev: $NodeType =:= NodeId)   : DAG[NodeId] = transform(n => f(n.asInstanceOf[NodeId]))(id => id)

      // ---------------------------------------------------------------------------
      def transformNode[A](id: NodeId)(f: A => A)(implicit ev: $NodeType <:< (NodeId, A)): Self =
        new DAG(
          nodes.mapIf(_._1 == id) { x => (x._1 -> f(x._2)).asInstanceOf[$NodeType] },
          edges,
          idResolver)

    // ===========================================================================
    // must ensure common nodes/edges have already been added on both sides
    def mergeBlindly(that: Self): Self = // used in Handler
      new DAG(
          (this.nodes ++ that.nodes).distinct, // TODO: opt
          (this.edges ++ that.edges).distinct,
          idResolver)

    // ---------------------------------------------------------------------------
    private def addNode(node: $NodeType, edge: Edge): Self =
      new DAG( // TODO: checks
         nodes :+ node,
         edges :+ edge,
         idResolver)

    // ---------------------------------------------------------------------------
    def replaceNode(target: $NodeType): Self = { // used in MetaPlan
      val targetId = idResolver(target)
      require(nodeIdSet.contains(targetId), (target, nodeIds.#@@))

      new DAG(
          nodes
            .map { node =>
              if (idResolver(node) == targetId) target
              else                              node },
          edges,
          idResolver)
    }

    // ---------------------------------------------------------------------------
    private[dag] def addEdge(edge: Edge): Self = {
      require(nodeIdSet.contains(edge._1), (edge._1, nodeIds.#@@))
      require(nodeIdSet.contains(edge._2), (edge._2, nodeIds.#@@))

      new DAG( // TODO: checks
         nodes,
         edges :+ edge,
         idResolver)
    }

    // ---------------------------------------------------------------------------
    private[dag] def removeNodes(targetNodeSet: Set[$NodeType]): Self = {
      val targetNodeIdSet = targetNodeSet.map(idResolver)
      require(
          targetNodeIdSet.diff(this.nodeIdSet).isEmpty,
          (this, targetNodeSet))

      new DAG(
        nodes.filterNot(targetNodeSet.contains),
        edges
          .filterNot { case (from, to) =>
            targetNodeIdSet.contains(from) ||
            targetNodeIdSet.contains(to  ) },
        idResolver)
    }
}

// ===========================================================================
object DAG {
  def EmptyIdDAG: IdDAG = new DAG[NodeId](Nil, Nil, identity)

  // ---------------------------------------------------------------------------
  def empty  [$NodeType](idResolver: $NodeType => NodeId)                 : DAG[$NodeType] = new DAG[$NodeType](Nil, Nil, idResolver)
  def trivial[$NodeType](idResolver: $NodeType => NodeId)(root: $NodeType): DAG[$NodeType] = new DAG[$NodeType](Seq(root), Nil, idResolver)

  // ---------------------------------------------------------------------------
  def from(pairs: Edge*): IdDAG =
    pairs
      .foldLeft(EmptyIdDAG) { (curr, pair) =>
        new DAG[NodeId](// TODO: reqs
          (curr.nodes ++ Seq(pair._1, pair._2)).distinct, // TODO: opt
           curr.edges :+ pair,
           identity) }

  // ---------------------------------------------------------------------------
  def idDag(nodes: Seq[DAGIn.DagTmp]*): IdDAG = DAGIn.idDag(nodes:_*) // still needed?
}

// ===========================================================================
