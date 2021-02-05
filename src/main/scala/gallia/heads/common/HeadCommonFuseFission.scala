package gallia.heads.common

import gallia._
import gallia.FunctionWrappers._
import gallia.actions.ActionsUUFuseFission._
import gallia.target.utils.TypedTargetQueryUtils._

// ===========================================================================
trait HeadCommonFuseFission[F <: HeadCommon[F]] { _: HeadCommon[F] =>
  import TSL.FuseFission._

  // ---------------------------------------------------------------------------
  @Max5
  def fission[O1: WTT](f1: Fission[O1]) = new {

    def as(a: KPathW, b: KPathW) = new {
      def using[D1: WTT, D2: WTT](f: O1 => (D1, D2))
        : Self2 = self2 :+ Fission2(resolve(f1), ttqkpath2[D1, D2](a, b), wrap12(f)) }

    // ---------------------------------------------------------------------------
    def as(a: KPathW, b: KPathW, c: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT](f: O1 => (D1, D2, D3))
        : Self2 = self2 :+ Fission3(resolve(f1), ttqkpath3[D1, D2, D3](a, b, c), wrap13(f)) }

  }

  // ===========================================================================
  def fuse(f1: KPathW, f2: KPathW)           : _Fuse2WV           = new _Fuse2WV(f1.value, f2.value) //TODO: path ok?
  def fuse(f1: KPathW, f2: KPathW, f3: KPathW): _Fuse3[WV, WV, WV] = fuse(_._explicit(f1.value), _._explicit(f2.value), _._explicit(f3.value)) // TODO

  // ---------------------------------------------------------------------------
  @Max5
  def fuse[O1: WTT, O2: WTT         ](f1: Fuse[O1], f2: Fuse[O2])               = new _Fuse2(f1, f2)
  def fuse[O1: WTT, O2: WTT, O3: WTT](f1: Fuse[O1], f2: Fuse[O2], f3: Fuse[O3]) = new _Fuse3(f1, f2, f3)

    // ===========================================================================
    class _Fuse2[O1: WTT, O2: WTT](f1: Fuse[O1], f2: Fuse[O2]) {
          def as(a: KPathW) = new {
            def using[D1: WTT](f: (O1, O2) => D1)
              : Self2 = self2 :+ Fuse2(resolve2(f1, f2), ttqkpath1[D1](a), wrap21(f)) } }

      // ---------------------------------------------------------------------------
      class _Fuse2WV(f1: KPath, f2: KPath) {
          def as(a: KPathW) = new {
            def using        (f: (WV, WV) =>     WV1 )                    : Self2 = self2 :+ Fuse2WV(TargetQueryUtils.tqkpath2(f1, f2), Left(a.value), false, wwwrap21a(f))
            def using        (f: (WV, WV) => Seq[WV1])(implicit di: DI)   : Self2 = self2 :+ Fuse2WV(TargetQueryUtils.tqkpath2(f1, f2), Left(a.value), true , wwwrap21a(f) )
            def using[D: WTT](f: (WV, WV) =>     WV2[D])                  : Self2 = self2 :+ Fuse2WV(TargetQueryUtils.tqkpath2(f1, f2), Right(ttqkpath1[D](a)), false, wwwrap21b(f) )
            def using[D: WTT](f: (WV, WV) => Seq[WV2[D]])(implicit di: DI): Self2 = self2 :+ Fuse2WV(TargetQueryUtils.tqkpath2(f1, f2), Right(ttqkpath1[D](a)), true , wwwrap21b(f) )
      } }

    // ---------------------------------------------------------------------------
    class _Fuse3[O1: WTT, O2: WTT, O3: WTT](f1: Fuse[O1], f2: Fuse[O2], f3: Fuse[O3]) {
        def as(a: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3) => D1): Self2 = self2 :+
            Fuse3(resolve3(f1, f2, f3), ttqkpath1[D1](a), wrap31(f)) } }

}

// ===========================================================================
