package gallia
package plans

import dag.DAG
import result.SuccessMetaResultNode

// ===========================================================================
object ActionPlanPopulator {

  def apply(asg: DAG[SuccessMetaResultNode]): ActionPlan =
    asg
      .transform4 { node =>
        asg
          .afferentNodes(node.id)
          .map (_.cls)
          .pipe(Clss.apply)
          .pipe(node.actionNode) }
      .pipe(ActionPlan.apply) }

// ===========================================================================
