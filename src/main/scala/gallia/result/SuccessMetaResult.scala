package gallia.result

import aptus.{Anything_, Seq_}

import gallia._
import gallia.dag._
import gallia.plans.ActionNode

// ===========================================================================
case class SuccessMetaResult(dag: DAG[SuccessMetaResultNode]) {
    def forceLeafClass: Cls = dag.leaves.force.one.cls
  }

  // ===========================================================================
  case class SuccessMetaResultNode(id: NodeId, origin: CallSite, action: ActionAN, cls: Cls) extends HasNodeId {

    def actionNode(afferents: Seq[Cls]): ActionNode =
      NodeMetaContext(afferents, cls, origin)
        .pipe { ctx =>
            ActionNode(id, action.atoms(ctx), ctx, origin)}

  }

// ===========================================================================
