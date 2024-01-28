package gallia
package plans

import aptus.Seq_

// ===========================================================================
case class ActionPlan(actionDag: DAG[ActionNode]) {
  def atomPlan: AtomPlan = AtomPlanPopulator(actionDag).pipe(new AtomPlan(_))

  // ---------------------------------------------------------------------------
  override def toString: String = formatDefault
    //dag.formatDot(node => node.formatDefault -> "white")
    def formatDefault: String = actionDag.kahnTraverseNodes.map(_.formatDefault).joinln }

// ===========================================================================
