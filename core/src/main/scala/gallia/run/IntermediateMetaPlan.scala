package gallia
package run

import aptus.{Anything_, String_, Seq_, Option_}

import plans._
import result._

// ===========================================================================
/** meta may still have failed here */
class IntermediateMetaPlan(dag: DAG[IntermediateMetaPlan.Node])
        extends gallia.dag.GalliaDAG [IntermediateMetaPlan.Node, CallSite, ActionAN](dag) {

      override def toString: String = formatDefault

        def formatDefault: String = dag.formatDefault
        def formatErrors : String = allErrors.map(_.format).#@@
          //graphviz.GraphVizUtils.IntermediateMetaResultNode_ ; { node => (node.formatDotLabel, node.formatDotColor) }

      // ===========================================================================
      /** left if meta failed */
      def intermediateEither: Either[MetaErrorResult, (SuccessMetaPlan, ActionPlan)] =
          if (!leafNode.isSuccess) Left(this)
          else
            dag
              .transform(_.successOpt.force)(_.id)
              .pipe { new SuccessMetaPlan(_) }
              .associateRight(_.actionPlan)
              .in.right

        // ---------------------------------------------------------------------------
        def forceNestedActionPlan: ActionPlan =
          intermediateEither match {
            case Left(errorneousMetaRun) => aptus.illegalArgument(
              errorneousMetaRun.allErrors.joinln.prepend("ActionPlanCreation:210114174456:"))
            case Right((_, actionPlan)) => actionPlan }

      // ===========================================================================
      def allErrors: Errs = dag.kahnTraverseNodes.flatMap(_.potentialResultSchema.errors)

        // ---------------------------------------------------------------------------
        def containsAllErrorMarkers(markers: Seq[String]): Boolean =
          markers.forall { marker =>
            allErrors.map(_.format).exists(_.contains(marker)) } // mostly for tests

      // ---------------------------------------------------------------------------
      def metaErrorOpt: Option[MetaError] =
        dag
          .kahnTraverseNodes
          .map(_.potentialResultSchema)
          .dropWhile(!_.isError)
          .headOption
          .map(_.errorOpt.get /* since isError */)
          .map(MetaError)

      // ---------------------------------------------------------------------------
      def forceLeafClass: Cls =
        leafNode.potentialResultSchema match {
          case PotentialResultSchema.UpstreamError          => aptus.illegalState("UpstreamError:201006134638") //TODO
          case PotentialResultSchema.Errors(values, origin) => aptus.illegalState(s"MetaErrors:201006134639:${values}:${origin}") //TODO
          case PotentialResultSchema.Success(value)         => value }

      // ===========================================================================
      private def leafNode: IntermediateMetaPlan.Node =
        dag
          .leaves
          .force.one /* since ASG */ }

  // ===========================================================================
  object IntermediateMetaPlan {
    case class Node(id: NodeId, origin: CallSite, actiona: ActionAN, potentialResultSchema: PotentialResultSchema)
        extends HasNodeId
        with    HasNodeContext[CallSite]
        with    dag.HasNodeTarget [ActionAN] {
      protected val ctxOpt = Some(origin)
      protected val target = actiona

      // ---------------------------------------------------------------------------
      def isSuccess: Boolean = potentialResultSchema.successOpt.isDefined

      def successOpt: Option[SuccessMetaPlan.Node] =
        potentialResultSchema
          .successOpt
          .map(SuccessMetaPlan.Node(id, origin, actiona, _)) } }

// ===========================================================================
