package gallia
package plans

import aptus.Anything_

import run._
import dag._
import env.ActionDag

// ===========================================================================
case class IntermediatePlan private[plans] (dag: ActionDag) {
  def run(): IntermediateMetaResult = IntermediatePlan.applyx(dag).pipe(IntermediateMetaResultNodeCreator(dag)) }

// ===========================================================================
private object IntermediatePlan {

    def applyx(dag: ActionDag): Map[NodeId, ResultSchema] = {
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
              resultSchema(inputs)(actionm) }

      // ---------------------------------------------------------------------------
      mut.toMap }

  // ===========================================================================
  private def resultSchema(inputs: Seq[ResultSchema])(actionm: ActionVN with ActionMN): ResultSchema =
      inputs
        .map(_.successOpt)
        .in.noneIf(_.exists(_.isEmpty)) // = none if any failure
        .map(_.flatten)
        .map(Clss.apply)
         match {
          case None       => ResultSchema.UpstreamError
          case Some(afferentClss) =>
            actionm.vldt(afferentClss) match {
              case Nil =>
                actionm
                  ._meta(afferentClss)
                  .tap { efferent =>
                    actionm._metaContext =
                      NodeMetaContext(afferentClss, efferent, CallSite(None, Nil)) }
                  .pipe(ResultSchema.Success.apply)
              case errors => ResultSchema.Errors(errors, actionm.callSite) } } }

// ===========================================================================
