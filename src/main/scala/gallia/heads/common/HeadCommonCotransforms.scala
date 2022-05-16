package gallia
package heads.common

import aptus.Anything_
import target.{HT, HT2, TtqKPath2}
import target.utils.TypedTargetQueryUtils._
import FunctionWrappers._
import actions.ActionsUUCotransforms._

// ===========================================================================
trait HeadCommonCotransforms[F <: HeadCommon[F]] { ignored: HeadCommon[F] => // TODO: t210826102833 - rework co-transforms

  def cotransform[O1: WTT](f1: Cotransform[O1]) = new { import TSL.Cotransform._
      private val origin = resolve(f1)

      def asOverwrite = new { def using[D1: WTT](f: O1 => D1): Self2 = self2 :+ Cotransform1to1(origin, Left(HT.parse[D1]), wrap11(f) ) }
                              def using[D1: WTT](f: O1 => D1): Self2 = self2 :+ Cotransform1to1(origin, Left(HT.parse[D1]), wrap11(f) )

      def as(a: KPathW)                       = new { def using[D1: WTT                  ](f: O1 =>  D1         ): Self2 = self2 :+ Cotransform1to1(origin, ttqkpath1[D1        ](a      ).in.right, wrap11(f)) }
      def as(a: KPathW, b: KPathW)            = new { def using[D1: WTT, D2: WTT         ](f: O1 => (D1, D2    )): Self2 = self2 :+ Cotransform1to2(origin, ttqkpath2[D1, D2    ](a, b   )         , wrap12(f)) }
      def as(a: KPathW, b: KPathW, c: KPathW) = new { def using[D1: WTT, D2: WTT, D3: WTT](f: O1 => (D1, D2, D3)): Self2 = self2 :+ Cotransform1to3(origin, ttqkpath3[D1, D2, D3](a, b, c)         , wrap13(f)) }
    }

    // ---------------------------------------------------------------------------
    @Max5
    def cotransform[O1: WTT, O2: WTT](f1: Cotransform[O1], f2: Cotransform[O2]) = new { import TSL.Cotransform._
      private val origin = resolve2(f1, f2)

      def asOverwrite = new { def using[D1: WTT, D2: WTT](f: (O1, O2) => (D1, D2)): Self2 = self2 :+ Cotransform2to2(origin, HT2.from[D1, D2].in.left, wrap22(f) ) }
                              def using[D1: WTT, D2: WTT](f: (O1, O2) => (D1, D2)): Self2 = self2 :+ Cotransform2to2(origin, HT2.from[D1, D2].in.left, wrap22(f) )

      def as(a: KPathW)                       = new { def using[D1: WTT                  ](f: (O1, O2) =>  D1         ) = self2 :+ Cotransform2to1(origin, ttqkpath1[D1        ](a      )         , wrap21(f)) }
      def as(a: KPathW, b: KPathW)            = new { def using[D1: WTT, D2: WTT         ](f: (O1, O2) => (D1, D2    )) = self2 :+ Cotransform2to2(origin, ttqkpath2[D1, D2    ](a, b   ).in.right, wrap22(f)) }
      def as(a: KPathW, b: KPathW, c: KPathW) = new { def using[D1: WTT, D2: WTT, D3: WTT](f: (O1, O2) => (D1, D2, D3)) = self2 :+ Cotransform2to3(origin, ttqkpath3[D1, D2, D3](a, b, c)         , wrap23(f)) }
    }

}

// ===========================================================================
