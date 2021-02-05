package gallia.plans

import aptus.Seq_
import gallia.dag.DAG

// ===========================================================================
case class ActionPlan(dag: DAG[ActionNode]) {
  def atomPlan: AtomPlan = AtomPlanPopulator(this)

  // ---------------------------------------------------------------------------
  override def toString: String = formatDefault
    def formatDefault: String = dag.kahnTraverseNodes.map(_.formatDefault).joinln
    //dag.formatDot(node => node.formatDefault -> "white")
}

// ===========================================================================
