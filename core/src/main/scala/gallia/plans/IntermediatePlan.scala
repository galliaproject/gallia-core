package gallia
package plans

import aptus.Anything_
import run._
import env._

// ===========================================================================
class IntermediatePlan private[gallia](dag: ActionDag)
      extends gallia.dag.GalliaDAG[ActionNodePair, Nothing, ActionVMN](dag) {
    dag
      .nodes
      .foreach { // TODO: move to transform3 (else may fail at runtime); may need classtag
         _.ensuring(!_.isNestingMetaPlaceholder) }

    // ---------------------------------------------------------------------------
    def run(): IntermediateMetaResult =
      IntermediatePlan
        .populateDataMap(dag)
        .pipe(IntermediatePlan.run(dag)) }

  // ===========================================================================
  object IntermediatePlan {

    private def run(dag: env.ActionDag)(dataMap: Map[NodeId, ResultSchema]): IntermediateMetaResult =
        dag
          .transform { _ .intermediateMetaResultNode(dataMap) }(newIdResolver = _.id)
          .pipe(new IntermediateMetaResult(_))

    // ===========================================================================
    private def populateDataMap(dag: env.ActionDag): Map[NodeId, ResultSchema] = {
        val mut = collection.mutable.Map[NodeId, ResultSchema]()

        // ---------------------------------------------------------------------------
        dag
          .kahnTraverseNodes // any topological order will do though
          .foreach { pair =>
            val inputs =
              dag
                .afferentIds(pair.id) // may be empty if root
                .map(mut.apply)

            mut +=
              pair.id ->
                resultSchema(inputs)(pair.actionvm) }

        // ---------------------------------------------------------------------------
        mut.toMap }

      // ===========================================================================
      private def resultSchema(inputs: Seq[ResultSchema])(actionvm: ActionVMN): ResultSchema =
        inputs
          .map(_.successOpt)
          .in.noneIf(_.exists(_.isEmpty)) // = none if any failure
          .map(_.flatten)
          .map(Clss.apply)
           match {
            case None       => ResultSchema.UpstreamError
            case Some(afferentClss) =>
              actionvm.vldt(afferentClss) match {
                case Nil =>
                  actionvm
                    ._meta(afferentClss)
                    .tap { efferent =>
                      actionvm._metaContext =
                        ActionMetaContext(afferentClss, efferent, CallSite(None, Nil)) }
                    .pipe(ResultSchema.Success.apply)
                case errors => ResultSchema.Errors(errors, actionvm.callSite) } } }

// ===========================================================================
