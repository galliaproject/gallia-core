package gallia
package result

import aptus.Seq_

import dag._
import plans.{Clss, ActionNode}

// ===========================================================================
case class SuccessMetaResult(dag: DAG[SuccessMetaResultNode]) {
    def forceLeafClass: Cls = dag.leaves.force.one.cls }

  // ===========================================================================
  case class SuccessMetaResultNode(id: NodeId, origin: CallSite, action: ActionAN, cls: Cls) extends HasNodeId {

    def actionNode(afferents: Clss): ActionNode =
      NodeMetaContext(afferents, cls, origin)
        .pipe { ctx =>
          ActionNode(id, action.atoms(ctx), ctx, origin)} }

// ===========================================================================
