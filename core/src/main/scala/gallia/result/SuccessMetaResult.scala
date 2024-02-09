package gallia
package result

import aptus.Seq_

import dag._
import plans.{Clss, ActionNode}

// ===========================================================================
/** meta must have succeeded here */
case class SuccessMetaResult(dag: DAG[SuccessMetaResultNode]) {
    def forceLeafClass: Cls = dag.leaves.force.one.cls }

  // ===========================================================================
  case class SuccessMetaResultNode(id: NodeId, origin: CallSite, actionan: ActionAN, cls: Cls) extends HasNodeId {
    def actionNode(afferents: Clss): ActionNode =
      NodeMetaContext(afferents, cls, origin)
        .pipe { ctx =>
          ActionNode(id, actionan.atoms(ctx), ctx, origin)} }

// ===========================================================================
