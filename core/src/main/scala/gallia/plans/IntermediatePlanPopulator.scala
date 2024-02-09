package gallia
package plans

// ===========================================================================
object IntermediatePlanPopulator {

  def apply(dag: env.ActionDag): IntermediatePlan = {
    dag
      .nodes
      .foreach { // TODO: move to transform3 (else may fail at runtime); may need classtag
         _.ensuring(!_.isNestingMetaPlaceholder) }

    IntermediatePlan(dag) } }

// ===========================================================================