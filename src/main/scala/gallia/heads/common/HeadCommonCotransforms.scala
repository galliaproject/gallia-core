package gallia.heads.common

import aptus.Anything_

import gallia._
import gallia.target.{HT,HT2}
import gallia.target.utils.TypedTargetQueryUtils._
import gallia.FunctionWrappers._
import gallia.actions.ActionsUUCotransforms._

// ===========================================================================
trait HeadCommonCotransforms[F <: HeadCommon[F]] { _: HeadCommon[F] =>

  def cotransform[O1: WTT](f1: Cotransform[O1]) = new { import TSL.Cotransform._
      private val tmp = resolve(f1)

      def asOverwrite = new { def using[D1: WTT](f: O1 => D1): Self2 =
        self2 :+ Cotransform1to1(tmp, HT.parse[D1].as.left, wrap11(f) ) }

      def as(a: KPathW)                       = new { def using[D1: WTT                  ](f: O1 =>  D1         ): Self2 = self2 :+ Cotransform1to1(tmp, ttqkpath1[D1        ](a      ).as.right, wrap11(f)) }
      def as(a: KPathW, b: KPathW)            = new { def using[D1: WTT, D2: WTT         ](f: O1 => (D1, D2    )): Self2 = self2 :+ Cotransform1to2(tmp, ttqkpath2[D1, D2    ](a, b   )         , wrap12(f)) }
      def as(a: KPathW, b: KPathW, c: KPathW) = new { def using[D1: WTT, D2: WTT, D3: WTT](f: O1 => (D1, D2, D3)): Self2 = self2 :+ Cotransform1to3(tmp, ttqkpath3[D1, D2, D3](a, b, c)         , wrap13(f)) }
    }

    // ---------------------------------------------------------------------------
    @Max5
    def cotransform[O1: WTT, O2: WTT](f1: Cotransform[O1], f2: Cotransform[O2]) = new { import TSL.Cotransform._
      private val tmp = resolve2(f1, f2)

      def asOverwrite = new { def using[D1: WTT, D2: WTT](f: (O1, O2) => (D1, D2)): Self2 =
        self2 :+ Cotransform2to2(tmp, HT2.from[D1, D2].as.left, wrap22(f) ) }

      def as(a: KPathW)                       = new { def using[D1: WTT                  ](f: (O1, O2) =>  D1         ) = self2 :+ Cotransform2to1(tmp, ttqkpath1[D1        ](a      )         , wrap21(f)) }
      def as(a: KPathW, b: KPathW)            = new { def using[D1: WTT, D2: WTT         ](f: (O1, O2) => (D1, D2    )) = self2 :+ Cotransform2to2(tmp, ttqkpath2[D1, D2    ](a, b   ).as.right, wrap22(f)) }
      def as(a: KPathW, b: KPathW, c: KPathW) = new { def using[D1: WTT, D2: WTT, D3: WTT](f: (O1, O2) => (D1, D2, D3)) = self2 :+ Cotransform2to3(tmp, ttqkpath3[D1, D2, D3](a, b, c)         , wrap23(f)) }
    }

}

// ===========================================================================
