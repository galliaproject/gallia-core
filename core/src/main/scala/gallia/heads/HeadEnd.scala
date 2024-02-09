package gallia
package heads

import aptus.Tuple2_

import env._
import plans._
import result._
import dag.LeafId
import run.MetaErrorResult

// ===========================================================================
case class HeadEnd private (leafId : LeafId) {
    def runDataOnly() = ???// TODO: t210106101459 - may be complicated by atoms that require pre/post schemata

    // ---------------------------------------------------------------------------
    def runMetaOnly(): RunResultM =
      initialMetaPlan
        .run().intermediateEither
        .map((SuccessResultM.apply _).tupled)
        .pipe(RunResultM)

    // ===========================================================================
    private[gallia] def runToNdt(): Either[MetaErrorResult, (SuccessMetaPlan, NDT)] =
        initialMetaPlan
          .run().intermediateEither
          .map(_.mapSecond(_.atomPlan.naiveRun()))

    // ---------------------------------------------------------------------------
    private[gallia] def run[$Data](): RunResult[SuccessResult[$Data], $Data] =
        initialMetaPlan
          .run().intermediateEither
          .map(_.mapSecond(_.atomPlan.naiveRun().value.asInstanceOf[$Data]))
          .map(x => new SuccessResult(x._1, x._2))
          .pipe(new RunResult(_))
  
      // ---------------------------------------------------------------------------
      def runu(): RunResultU =
        initialMetaPlan
          .run().intermediateEither
          .map(_.mapSecond(_.atomPlan.naiveRun().forceO))
          .map((SuccessResultU.apply _).tupled)
          .pipe(RunResultU)
  
      // ---------------------------------------------------------------------------
      def runz(): RunResultZ =
        initialMetaPlan
          .run().intermediateEither
          .map(_.mapSecond(_.atomPlan.naiveRun().forceZ))
          .map((SuccessResultZ.apply _).tupled)
          .pipe(RunResultZ)
  
      // ---------------------------------------------------------------------------
      def runv[T](): RunResultV[T] =
        initialMetaPlan
          .run().intermediateEither
          .map(_.mapSecond(_.atomPlan.naiveRun().forceT[T]))
          .map(x => SuccessResultV[T](x._1, x._2)) // TODO: t210117104246 - can use .tupled?
          .pipe(RunResultV[T])

    // ===========================================================================
    private def initialMetaPlan: InitialMetaPlan =
      Env
        .retrieveDagFromNode(leafId)
        .afferentSubGraph   (leafId)
        .pipe(new InitialMetaPlan(_))

  }

  // ---------------------------------------------------------------------------
  object HeadEnd {
    def build(leafId: LeafId): HeadEnd = HeadEnd(leafId) }

// ===========================================================================
