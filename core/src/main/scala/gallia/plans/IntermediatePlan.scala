package gallia
package plans

import aptus.Anything_

import run._
import dag._
import env.ActionDag

// ===========================================================================
case class IntermediatePlan private[plans] (dag: ActionDag) {

    def run(): IntermediateMetaResult = {
      val mut = collection.mutable.Map[NodeId, ResultSchema]()

      // ---------------------------------------------------------------------------
      dag
        .kahnTraverseNodes // any topological order will do though
        .foreach { case (id, actionm) =>
          val inputs =
            dag
              .afferentIds(id) // may be empty if root
              .map(mut.apply)

          mut +=
            id ->
              IntermediatePlan.node(inputs)(actionm) }

      // ---------------------------------------------------------------------------
      mut
        .toMap
        .pipe(IntermediateMetaResultNodeCreator(dag)) } }

  // ===========================================================================
  object IntermediatePlan  {

    private def node(inputs: Seq[ResultSchema])(actionm: ActionVN with ActionMN): ResultSchema =
      inputs
        .map(_.successOpt)
        .in.noneIf(_.exists(_.isEmpty)) // = none if any failure
        .map(_.flatten)
        .map(Clss.apply)
         match {
          case None       => ResultSchema.UpstreamError
          case Some(clss) =>
            actionm.vldt(clss) match {
              case Nil =>
                actionm
                  ._meta(clss)
                  .tap { efferent =>
                    actionm._metaContext =
                      NodeMetaContext(afferents = clss, efferent, CallSite(None, Nil)) }
                  .pipe(ResultSchema.Success.apply)
              case errors => ResultSchema.Errors(errors, actionm.callSite) } } }

// ===========================================================================
