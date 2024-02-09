package gallia
package result

import aptus.Seq_
import aptus.Size

import dag._
import plans.{Clss, ActionNode, ActionPlan}

// ===========================================================================
/** meta must have succeeded here */
class SuccessMetaResult(dag: DAG[SuccessMetaResultNode])
      extends GalliaDAG[SuccessMetaResultNode](dag) {

    def leavesCount   : Size = dag.leaves.size
    def forceLeafClass: Cls  = dag.leaves.force.one.cls

    def actionPlan: ActionPlan =
      dag
        .transform4 { node =>
          dag
            .afferentNodes(node.id)
            .map (_.cls)
            .pipe(Clss.apply)
            .pipe(node.actionNode) }
        .pipe { new ActionPlan(_) } }

  // ===========================================================================
  case class SuccessMetaResultNode(id: NodeId, origin: CallSite, actionan: ActionAN, cls: Cls) extends HasNodeId {
    def actionNode(afferents: Clss): ActionNode =
      ActionMetaContext(afferents, cls, origin)
        .pipe { ctx =>
          ActionNode(id, actionan.atoms(ctx), ctx)} }

// ===========================================================================
