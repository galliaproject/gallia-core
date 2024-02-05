package gallia
package plans

import aptus.Seq_

// ===========================================================================
case class ActionPlan(actionNodeDag: DAG[ActionNode]) {
  def atomPlan: AtomPlan = AtomPlanPopulator(actionNodeDag).pipe(new AtomPlan(_))

  // ---------------------------------------------------------------------------
  override def toString: String = formatDefault
    //dag.formatDot(node => node.formatDefault -> "white")
    def formatDefault: String = actionNodeDag.kahnTraverseNodes.map(_.formatDefault).joinln }

// ===========================================================================
