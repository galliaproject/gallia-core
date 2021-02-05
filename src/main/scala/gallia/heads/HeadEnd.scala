package gallia.heads

import aptus.Anything_
import aptus.Tuple2_

import gallia.env._
import gallia.plans._
import gallia.result._
import gallia.dag.LeafId

// ===========================================================================
case class HeadEnd private (leafId : LeafId, env: Env) {

    def runu(): RunResultU =
      adagm
        .run().either
        .map(_.mapSecond(_.atomPlan.naiveRun().forceO))
        .map(SuccessResultU.tupled)
        .thn(RunResultU)

    // ---------------------------------------------------------------------------
    def runz(): RunResultZ =
      adagm
        .run().either
        .map(_.mapSecond(_.atomPlan.naiveRun().forceZ))
        .map(SuccessResultZ.tupled)
        .thn(RunResultZ)

    // ---------------------------------------------------------------------------
    def runv[T](): RunResultV[T] =
      adagm
        .run().either
        .map(_.mapSecond(_.atomPlan.naiveRun().forceT[T]))
        .map(x => SuccessResultV[T](x._1, x._2)) // TODO: t210117104246 - can use .tupled?
        .thn(RunResultV[T])

    // ===========================================================================
    private def adagm: IntermediatePlan =
      env
        .retrieveDagFromNode(leafId)
        .afferentSubGraph   (leafId)
        .thn(IntermediatePlanPopulator.apply)

    // ===========================================================================
    def runMetaOnly()           = ???// TODO: t210106101457 - wouldn't it be nice
    def runDataOnly(): gallia.Objs = ???// TODO: t210106101459 - may be complicated by atoms that require pre/post schemata
  }

// ===========================================================================
