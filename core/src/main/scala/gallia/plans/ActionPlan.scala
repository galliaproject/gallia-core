package gallia
package plans

import aptus.Seq_
import dag.Edge

// ===========================================================================
class ActionPlan(actionNodeDag: DAG[ActionNode])
      extends dag.GalliaDAG[ActionNode, ActionMetaContext, Seq[Atom]](
        actionNodeDag) {

    def atomPlan: AtomPlan =
      ActionPlan
        .atomNodesDag(actionNodeDag)
        .pipe(new AtomPlan(_))

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      //dag.formatDot(node => node.formatDefault -> "white")
      def formatDefault: String = actionNodeDag.kahnTraverseNodes.map(_.formatDefault).joinln }

  // ===========================================================================
  object ActionPlan {

    private def atomNodesDag(actionDag: DAG[ActionNode]): DAG[AtomNode] = {

      def efferentNodeIds(id: NodeId): Seq[NodeId] =
        actionDag
          .efferentNodes(id)
          .flatMap { efferentNode =>
            if (efferentNode.atoms.nonEmpty) Seq(efferentNode.id)
            else                             efferentNodeIds(efferentNode.id) } // recursive call

      // ===========================================================================
      val atomNodes: Seq[AtomNode] =
        actionDag
          .nodes
          .filterNot(_.atoms.isEmpty) // eg ValidateX or OutputX
          .flatMap  (_.atomNodes)

      // ---------------------------------------------------------------------------
      val atomEdges: Seq[Edge] =
        actionDag
          .nodes
          .filterNot(_.atoms.isEmpty) // eg ValidateX or OutputX
          .flatMap { actionNode =>
            val newNodeIds = actionNode.atomNodeIds

            // internal to the action (there may be more than one atom per action)
            val internalEdges: Seq[Edge] =
              newNodeIds.ensuring(_.nonEmpty) match {
                case Seq(_) => Nil
                case mult   => mult.slidingPairs }

            val efferentEdges: Seq[Edge] =
              efferentNodeIds(actionNode.id) // arbirarily over afferent, though easier to compute since can use 0 as index
                .map { efferentId =>
                  newNodeIds.last ->
                    atomId(efferentId, 0) }

            internalEdges ++ efferentEdges }

      // ---------------------------------------------------------------------------
      new DAG[AtomNode](atomNodes, atomEdges, _.id) } }

// ===========================================================================
