package gallia
package run

import aptus.{Anything_, String_, Seq_, Option_}

import plans._
import result._

// ===========================================================================
/** meta may still have failed here */
class IntermediateMetaResult(dag: DAG[IntermediateMetaResultNode])
        extends gallia.dag.GalliaDAG[IntermediateMetaResultNode, CallSite, ActionAN](dag) {

      override def toString: String = formatDefault

        def formatDefault: String = dag.formatDefault
        def formatErrors : String = allErrors.map(_.format).#@@
          //graphviz.GraphVizUtils.IntermediateMetaResultNode_ ; { node => (node.formatDotLabel, node.formatDotColor) }

      // ===========================================================================
      /** left if meta failed */
      def intermediateEither: Either[MetaErrorResult, (SuccessMetaResult, ActionPlan)] =
          if (!leafNode.isSuccess) Left(this)
          else
            dag
              .transform(_.successOpt.force)(_.id)
              .pipe { new SuccessMetaResult(_) }
              .associateRight(_.actionPlan)
              .in.right

        // ---------------------------------------------------------------------------
        def forceNestedActionPlan: ActionPlan =
          intermediateEither match {
            case Left(errorneousMetaRun) => aptus.illegalArgument(
              errorneousMetaRun.allErrors.joinln.prepend("ActionPlanCreation:210114174456:"))
            case Right((_, actionPlan)) => actionPlan }

      // ===========================================================================
      def allErrors: Errs = dag.kahnTraverseNodes.flatMap(_.result.errors)

        // ---------------------------------------------------------------------------
        def containsAllErrorMarkers(markers: Seq[String]): Boolean =
          markers.forall { marker =>
            allErrors.map(_.format).exists(_.contains(marker)) } // mostly for tests

      // ---------------------------------------------------------------------------
      def metaErrorOpt: Option[MetaError] =
        dag
          .kahnTraverseNodes
          .map(_.result)
          .dropWhile(!_.isError)
          .headOption
          .map(_.errorOpt.get /* since isError */)
          .map(MetaError)

      // ---------------------------------------------------------------------------
      def forceLeafClass: Cls  = leafNode.result match {
          case ResultSchema.UpstreamError          => aptus.illegalState("UpstreamError:201006134638") //TODO
          case ResultSchema.Errors(values, origin) => aptus.illegalState(s"MetaErrors:201006134639:${values}:${origin}") //TODO
          case ResultSchema.Success(value)         => value }

      // ===========================================================================
      private def leafNode: IntermediateMetaResultNode =
        dag
          .leaves
          .force.one /* since ASG */ }

  // ===========================================================================
  case class IntermediateMetaResultNode(id: NodeId, origin: CallSite, actiona: ActionAN, result: ResultSchema)
      extends HasNodeId
      with    HasNodeContext[CallSite]
      with    dag.HasNodeTarget [ActionAN] {
    protected val ctxOpt = Some(origin)
    protected val target = actiona

    // ---------------------------------------------------------------------------
    def isSuccess: Boolean = result.successOpt.isDefined

    def successOpt: Option[SuccessMetaResultNode] =
      result
        .successOpt
        .map(SuccessMetaResultNode(id, origin, actiona, _)) }

// ===========================================================================
