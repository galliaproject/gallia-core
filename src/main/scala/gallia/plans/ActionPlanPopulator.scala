package gallia.plans

import gallia._
import gallia.dag.DAG
import gallia.result.SuccessMetaResultNode

// ===========================================================================
object ActionPlanPopulator {

  def apply(asg: DAG[SuccessMetaResultNode]): ActionPlan =
    asg
      .transform4 { node =>
        val afferents: Seq[Cls] = asg.afferentNodes(node.id).map(_.cls)

        node.actionNode(afferents) }
      .pipe(ActionPlan.apply)

}

// ===========================================================================
