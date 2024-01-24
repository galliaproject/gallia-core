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

    private def node(inputs: Seq[ResultSchema])(actionm: ActionVMN): ResultSchema =
      inputs
        .map(_.successOpt)
        .in.noneIf(_.exists(_.isEmpty)) // = none if any failure
        .map(_.flatten)
        .map(Clss.apply)
         match {
          case None       => ResultSchema.UpstreamError
          case Some(clss) =>
            actionm.vldt(clss) match {
              case Nil    => ResultSchema.Success(actionm._meta(clss).tap { actionm.resultCls = _ /* TODO: relates to t201214105653 hack */ })
              case errors => ResultSchema.Errors(errors, actionm.callSite) } } }

// ===========================================================================
