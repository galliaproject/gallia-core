package gallia
package plans

import aptus.Anything_

import env.ActionDag
import heads.HeadsNestingHandler.NestingMetaPlaceholder

// ===========================================================================
object IntermediatePlanPopulator {

  def apply(dag: ActionDag): IntermediatePlan = {
    dag
      .nodes
      .foreach { // TODO: move to transform3 (else may fail at runtime); may need classtag
         _.assert(!_._2.isInstanceOf[NestingMetaPlaceholder]) }

    IntermediatePlan(dag)
  }

}

// ===========================================================================