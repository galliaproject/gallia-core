package gallia.plans

import aptus.Anything_

import gallia._
import gallia.run._
import gallia.dag._
import gallia.env.ActionDag

// ===========================================================================
case class IntermediatePlan private (dag: ActionDag) {

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
        .thn(IntermediateMetaResultNodeCreator(dag))
    }

  }

  // ===========================================================================
  object IntermediatePlan  {

    private def node(inputs: Seq[ResultSchema])(actionm: ActionVMN): ResultSchema =
      inputs
        .map(_.successOpt)
        .in.noneIf(_.exists(_.isEmpty)) // = none if any failure
        .map(_.flatten)
         match {
          case None       => ResultSchema.UpstreamError
          case Some(clss) =>
            actionm.vldt(clss) match {
              case Nil    => ResultSchema.Success(actionm._meta(clss).sideEffect { actionm.resultCls = _ /* TODO: relates to t201214105653 hack */ })
              case errors => ResultSchema.Errors(errors, actionm.callSite) } }

  }

// ===========================================================================