package gallia.plans

import aptus.{Anything_, String_, Seq_}

import gallia.dag._

// ===========================================================================
private[plans] object AtomPlanPopulator {

  def apply(plan: ActionPlan): AtomPlan = { import plan.dag

    def atomId(id: NodeId, index: Int): NodeId = id.append(s"-${index}" /* TODO: not if only one? */)

    // ---------------------------------------------------------------------------
    def _newNodeIds(node: ActionNode): Seq[NodeId] =
      node
        .atoms
        .assert(_.nonEmpty)
        .zipWithIndex
        .map { case (atom, index) =>
          atomId(node.id, index) }

    // ---------------------------------------------------------------------------
    def efferentNodeIds(id: NodeId): Seq[NodeId] =
      dag
        .efferentNodes(id)
        .flatMap { efferentNode =>
          if (efferentNode.atoms.nonEmpty) Seq(efferentNode.id)
          else                             efferentNodeIds(efferentNode.id) } // recursive call

    // ===========================================================================
    val nodes2: Seq[AtomNode] =
      dag
        .nodes
        .filterNot(_.atoms.isEmpty) // eg ValidateX or OutputX
        .flatMap { node =>
          _newNodeIds(node)
            .zip(node.atoms)
            .map { case (newNodeId, atom) =>
              node.atomNode(newNodeId, atom) } }

    // ---------------------------------------------------------------------------
    val edges2: Seq[Edge] =
      dag
        .nodes
        .filterNot(_.atoms.isEmpty) // eg ValidateX or OutputX
        .flatMap { node =>
          val newNodeIds = _newNodeIds(node)

          val internalEdges: Seq[Edge] =
            newNodeIds match {
              case Seq(_) => Nil
              case mult   => mult.sliding(2).map(_.force.tuple2).toSeq }

          val externalEdges: Seq[Edge] =
            efferentNodeIds(node.id) // arbirarily over afferent, though easier to compute since can use 0 as index
              .map { efferentId =>
                newNodeIds.last -> atomId(efferentId, 0) }

          internalEdges ++ externalEdges
        }

    // ---------------------------------------------------------------------------
    AtomPlan(new DAG[AtomNode](nodes2, edges2, _.id))
  }
}

// ===========================================================================
