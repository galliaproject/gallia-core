package gallia
package heads
package common

import aptus.Anything_
import target.{TypeDuo, TypeDuo2}
import target.utils.TypedTargetQueryUtils._
import FunctionWrappers._
import actions.common.ActionsCommonCotransforms._

// ===========================================================================
trait HeadCommonCotransforms[F <: HeadCommon[F]] { ignored: HeadCommon[F] => // TODO: t210826102833 - rework co-transforms

  def cotransform[O1: WTT](f1: Cotransform[O1]) = new _Cotransform1(f1)
     final class _Cotransform1[O1: WTT] private[heads] (f1: Cotransform[O1]) { import TSL.Cotransform._
      private val origin = resolve(f1)

      def asOverwrite = new _AsOverwrite

      final class _AsOverwrite private[heads] { def using[D1: WTT](f: O1 => D1): Self2 = self2 :+ Cotransform1to1(origin, Left(TypeDuo.build[D1]), wrap11(f) ) }
                                                def using[D1: WTT](f: O1 => D1): Self2 = self2 :+ Cotransform1to1(origin, Left(TypeDuo.build[D1]), wrap11(f) )

      def as(a: KPathW)                       = new _As1(a)
      def as(a: KPathW, b: KPathW)            = new _As2(a, b)
      def as(a: KPathW, b: KPathW, c: KPathW) = new _As3(a, b, c)

        final class _As1(a: KPathW)                       { def using[D1: WTT                  ](f: O1 =>  D1         ): Self2 = self2 :+ Cotransform1to1(origin, ttqkpath1[D1        ](a      ).in.right, wrap11(f)) }
        final class _As2(a: KPathW, b: KPathW)            { def using[D1: WTT, D2: WTT         ](f: O1 => (D1, D2    )): Self2 = self2 :+ Cotransform1to2(origin, ttqkpath2[D1, D2    ](a, b   )         , wrap12(f)) }
        final class _As3(a: KPathW, b: KPathW, c: KPathW) { def using[D1: WTT, D2: WTT, D3: WTT](f: O1 => (D1, D2, D3)): Self2 = self2 :+ Cotransform1to3(origin, ttqkpath3[D1, D2, D3](a, b, c)         , wrap13(f)) } }

    // ---------------------------------------------------------------------------
    @Max5
    def cotransform[O1: WTT, O2: WTT](f1: Cotransform[O1], f2: Cotransform[O2]) = new _Cotransform2(f1, f2)
     final class _Cotransform2[O1: WTT, O2: WTT] private[heads] (f1: Cotransform[O1], f2: Cotransform[O2]) { import TSL.Cotransform._
      private val origin = resolve2(f1, f2)

      def asOverwrite = new _AsOverwrite

      final class _AsOverwrite private[heads] { def using[D1: WTT, D2: WTT](f: (O1, O2) => (D1, D2)): Self2 = self2 :+ Cotransform2to2(origin, TypeDuo2.from[D1, D2].in.left, wrap22(f) ) }
                                                def using[D1: WTT, D2: WTT](f: (O1, O2) => (D1, D2)): Self2 = self2 :+ Cotransform2to2(origin, TypeDuo2.from[D1, D2].in.left, wrap22(f) )

      def as(a: KPathW)                       = new _As1(a)
      def as(a: KPathW, b: KPathW)            = new _As2(a, b)
      def as(a: KPathW, b: KPathW, c: KPathW) = new _As3(a, b, c)

        final class _As1(a: KPathW)                       { def using[D1: WTT                  ](f: (O1, O2) =>  D1         ) = self2 :+ Cotransform2to1(origin, ttqkpath1[D1        ](a      )         , wrap21(f)) }
        final class _As2(a: KPathW, b: KPathW)            { def using[D1: WTT, D2: WTT         ](f: (O1, O2) => (D1, D2    )) = self2 :+ Cotransform2to2(origin, ttqkpath2[D1, D2    ](a, b   ).in.right, wrap22(f)) }
        final class _As3(a: KPathW, b: KPathW, c: KPathW) { def using[D1: WTT, D2: WTT, D3: WTT](f: (O1, O2) => (D1, D2, D3)) = self2 :+ Cotransform2to3(origin, ttqkpath3[D1, D2, D3](a, b, c)         , wrap23(f)) } } }

// ===========================================================================
