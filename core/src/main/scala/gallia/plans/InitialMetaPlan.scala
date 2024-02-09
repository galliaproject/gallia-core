package gallia
package plans

import aptus.Anything_
import run._

// ===========================================================================
class InitialMetaPlan private[gallia](actionMetaDag: ActionMetaDag)
      extends dag.GalliaDAG[ActionMetaNode, Nothing, ActionVMN](actionMetaDag) {
    actionMetaDag
      .nodes
      .foreach { // TODO: move to transform3 (else may fail at runtime); may need classtag
         _.ensuring(!_.isNestingMetaPlaceholder) }

    // ---------------------------------------------------------------------------
    def run(): IntermediateMetaPlan =
      InitialMetaPlan
        .populateDataMap(actionMetaDag)
        .pipe(InitialMetaPlan.run(actionMetaDag)) }

  // ===========================================================================
  object InitialMetaPlan {

    private def run(actionMetaDag: ActionMetaDag)(dataMap: Map[NodeId, ResultSchema]): IntermediateMetaPlan =
        actionMetaDag
          .transform { _ .intermediateMetaPlanNode(dataMap) }(newIdResolver = _.id)
          .pipe(new IntermediateMetaPlan(_))

    // ===========================================================================
    private def populateDataMap(actionMetaDag: ActionMetaDag): Map[NodeId, ResultSchema] = {
        val mut = collection.mutable.Map[NodeId, ResultSchema]()

        // ---------------------------------------------------------------------------
        actionMetaDag
          .kahnTraverseNodes // any topological order will do though
          .foreach { pair =>
            val inputs =
              actionMetaDag
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
