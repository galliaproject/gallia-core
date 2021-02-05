package gallia.actions.utils

import aptus.Tuple2_

import gallia._
import gallia.plans._
import gallia.dag.RootId
import gallia.heads.HeadsNestingHandler

// ===========================================================================
class NestedTransform2(adag: MetaPlan, val rootId1: RootId, val rootId2: RootId) { // TODO: as a peer of MetaPlan rather?

    def vldt(c: Cls, kpath1: KPath, kpath2: KPath): Errs = {
      val a = c.field_(kpath1).flatMap(_.info.nestingTypeOpt)
      val b = c.field_(kpath2).flatMap(_.info.nestingTypeOpt)

      (a, b).toOptionalTuple
        .map { case (x, y) =>
          adag.runMeta(rootId1, rootId2, x, y) }
        .toSeq.flatMap(_.allErrors)
    }

    // ---------------------------------------------------------------------------
    def generateMeta(c: Cls, from1: KPath, optional1: Boolean, from2: KPath, optional2: Boolean): Cls =
      adag
        .runMeta(
            rootId1, rootId2,
            c.forceNestedClass(from1), c.forceNestedClass(from2))
        .forceLeafClass

    // ---------------------------------------------------------------------------
    // data

    //def dataUu2u(c1: Cls, optional1: Boolean, c2: Cls, optional2: Boolean): _ff21 = actionPlan(c1, c2).atomPlan.FromNesting.uu2u(optional1, optional2)

    // ===========================================================================
    private def actionPlan(c1: Cls, c2: Cls): ActionPlan =
      adag
        .runMeta(rootId1, rootId2, c1, c2)
        .forceActionPlan

  }

  // ===========================================================================
  object NestedTransform2 {
    def parseUUtoU(f: (HeadU, HeadU) => HeadU): NestedTransform2 = {
      val (rootIdPair, dag, leafId) = HeadsNestingHandler.uuToU(f)
      new NestedTransform2(MetaPlan(dag), rootIdPair._1, rootIdPair._2)
    }
  }

// ===========================================================================
