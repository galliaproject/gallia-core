package gallia
package actions
package utils

import aptus.Tuple2_

import plans._
import dag.RootId
import heads.HeadsNestingHandler

// ===========================================================================
class NestedTransform2(adag: NestedMetaPlan, val rootId1: RootId, val rootId2: RootId) { // TODO: as a peer of MetaPlan rather?

    def vldt(c: Cls, kpath1: KPath, kpath2: KPath): Errs = {
      val a = c.field_(kpath1).flatMap(_.nestedClassOpt)
      val b = c.field_(kpath2).flatMap(_.nestedClassOpt)

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
        .forceNestedActionPlan

  }

  // ===========================================================================
  object NestedTransform2 {
    def parseUUtoU(f: (HeadU, HeadU) => HeadU): NestedTransform2 = {
      val (rootIdPair, dag, leafId) = HeadsNestingHandler.uuToU(f)
      new NestedTransform2(NestedMetaPlan(dag), rootIdPair._1, rootIdPair._2) } }

// ===========================================================================
