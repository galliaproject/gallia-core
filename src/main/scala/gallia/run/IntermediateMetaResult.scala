package gallia.run

import aptus.{Anything_, String_, Seq_}
import aptus.Option_

import gallia._
import gallia.plans._
import gallia.result._

// ===========================================================================
case class IntermediateMetaResult(dag: gallia.dag.DAG[IntermediateMetaResultNode]) {

      override def toString: String = formatDefault

        def formatDefault: String = dag.formatDefault
          //gallia.graphviz.GraphVizUtils.IntermediateMetaResultNode_ ; { node => (node.formatDotLabel, node.formatDotColor) }

      // ===========================================================================
      def forceActionPlan = successOpt match {
          case None          => aptus.illegalArgument(allErrors.joinln.prepend("TODO:210114174456"))
          case Some(success) => ActionPlanPopulator(success.dag) }

      // ---------------------------------------------------------------------------
      def successOpt: Option[SuccessMetaResult] = if (leafNode.isSuccess) Some(_successMetaResult) else None

        private def _successMetaResult =
          dag
            .transform(_.successOpt.force)(_.id)
            .thn(SuccessMetaResult.apply)

      // ---------------------------------------------------------------------------
      def either: Either[MetaErrorResult, (SuccessMetaResult, ActionPlan)] =
        successOpt match {
            case None                             => Left (this)
            case Some(success: SuccessMetaResult) => Right(success, ActionPlanPopulator(success.dag)) }

      // ---------------------------------------------------------------------------
      def allErrors: Errs = dag.kahnTraverseNodes.flatMap(_.result.errors)
        def containsAllErrorMarkers(markers: Seq[String]): Boolean = markers.forall(allErrors.toString.contains) // mostly for tests        

      // ---------------------------------------------------------------------------
      def metaErrorOpt: Option[MetaError] =
        dag
          .kahnTraverseNodes
          .map(_.result)
          .dropWhile(!_.isError)
          .headOption
          .map(_.errorOpt.get /* since isError */)
          .map(gallia.MetaError)

      // ---------------------------------------------------------------------------
      def forceLeafClass: Cls  = leafNode.result match {
          case ResultSchema.UpstreamError          => aptus.illegalState("UpstreamError:201006134638") //TODO
          case ResultSchema.Errors(values, origin) => aptus.illegalState(s"Errors:201006134639:${values}:${origin}") //TODO
          case ResultSchema.Success(value)         => value }

      // ===========================================================================
      private def leafNode: IntermediateMetaResultNode =
        dag
          .leaves
          .force.one // since ASG
    }

  // ===========================================================================
  case class IntermediateMetaResultNode(id: NodeId, origin: CallSite, action: ActionAN, result: ResultSchema) extends HasNodeId {
    def isSuccess: Boolean = result.successOpt.isDefined

    def successOpt: Option[SuccessMetaResultNode] =
      result
        .successOpt
        .map(SuccessMetaResultNode(id, origin, action, _))
  }

// ===========================================================================
