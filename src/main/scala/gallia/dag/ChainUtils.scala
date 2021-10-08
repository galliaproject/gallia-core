package gallia.dag

import aptus.Anything_

// ===========================================================================
private object ChainUtils {

  def chain[$NodeType](dag: DAG[$NodeType])(nodeId: NodeId, node: $NodeType, skipAfferentCheck: Boolean = true): Chain[$NodeType] = {
    import dag._

    require(
        nodeIdSet.contains(nodeId),
        (nodeId, this))

    require(
        skipAfferentCheck || afferentCount(nodeId) <= 1,
        (nodeId, afferentIds(nodeId)))

    Chain(
      Seq(node) ++
      (dag.efferentIds(nodeId) match {
        case Nil       => Nil
        case Seq(nextId) =>
          if (afferentCount(nextId) > 1) Nil
          else                           this.chain(dag)(nextId, dag.lookup(nextId), skipAfferentCheck).nodes
        case fork      => Nil
      }),
      dag.idResolver)
  }

  // ===========================================================================
  // TODO: tailrec (worth it here though?)
  def chainTraversal[$NodeType](dag: DAG[$NodeType])(rootIds: Seq[NodeId]): Seq[$NodeType] = {
    val visitedNodes: Seq[$NodeType] =
      rootIds
        .map { x =>
          chain(dag)(x, dag.lookup(x), skipAfferentCheck = true) }
        .flatMap(_.nodes)

    val nextDag = dag.removeNodes(visitedNodes.toSet) // 201123165502 - may result in isolates... (each fork)

    visitedNodes ++
    nextDag
      .rootIds
      .in.noneIf(_.isEmpty)
      .toSeq.flatMap(
        chainTraversal(nextDag))
  }

}

// ===========================================================================
