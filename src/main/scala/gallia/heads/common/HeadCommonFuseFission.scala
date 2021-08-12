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
  def fission[O1: WTT](o1: Fission[O1]) = new {

    def as(d1: KPathW, d2: KPathW) = new {
      def using[D1: WTT, D2: WTT](f: O1 => (D1, D2))
        : Self2 = self2 :+ Fission2(resolve(o1), ttqkpath2[D1, D2](d1, d2), wrap12(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT](f: O1 => (D1, D2, D3))
        : Self2 = self2 :+ Fission3(resolve(o1), ttqkpath3[D1, D2, D3](d1, d2, d3), wrap13(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT](f: O1 => (D1, D2, D3, D4))
        : Self2 = self2 :+
          Fission4(resolve(o1), ttqkpath4[D1, D2, D3, D4](d1, d2, d3, d4), wrap14(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT](f: O1 => (D1, D2, D3, D4, D5))
        : Self2 = self2 :+
          Fission5(resolve(o1), ttqkpath5[D1, D2, D3, D4, D5](d1, d2, d3, d4, d5), wrap15(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT](f: O1 => (D1, D2, D3, D4, D5, D6))
        : Self2 = self2 :+
          Fission6(resolve(o1), ttqkpath6[D1, D2, D3, D4, D5, D6](d1, d2, d3, d4, d5, d6), wrap16(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7))
        : Self2 = self2 :+
          Fission7(resolve(o1), ttqkpath7[D1, D2, D3, D4, D5, D6, D7](d1, d2, d3, d4, d5, d6, d7), wrap17(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8))
        : Self2 = self2 :+
          Fission8(resolve(o1), ttqkpath8[D1, D2, D3, D4, D5, D6, D7, D8](d1, d2, d3, d4, d5, d6, d7, d8), wrap18(f)) }
    
    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW, d9: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT, D9: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8, D9))
        : Self2 = self2 :+
          Fission9(resolve(o1), ttqkpath9[D1, D2, D3, D4, D5, D6, D7, D8, D9](d1, d2, d3, d4, d5, d6, d7, d8, d9), wrap19(f)) }
        
    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW, d9: KPathW, d10: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT, D9: WTT, D10: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8, D9, D10))
        : Self2 = self2 :+
          Fission10(resolve(o1), ttqkpath9[D1, D2, D3, D4, D5, D6, D7, D8, D9, D10](d1, d2, d3, d4, d5, d6, d7, d8, d9, d10), wrap1A(f)) }
        
  }

  // ===========================================================================
  def fuse(f1: KPathW, f2: KPathW)            : _Fuse2WV           = new _Fuse2WV(f1.value, f2.value) //TODO: path ok?
  def fuse(f1: KPathW, f2: KPathW, f3: KPathW): _Fuse3[WV, WV, WV] = fuse(_._explicit(f1.value), _._explicit(f2.value), _._explicit(f3.value)) // TODO

  // ---------------------------------------------------------------------------
  @Max5
  def fuse[O1: WTT, O2: WTT](o1: Fuse[O1], o2: Fuse[O2]) = new _Fuse2(o1, o2)

      def fuse[O1: WTT, O2: WTT, O3: WTT]                                             (o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3])                                                                       = new _Fuse3(o1, o2, o3)
      def fuse[O1: WTT, O2: WTT, O3: WTT, O4: WTT]                                    (o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3], o4: Fuse[O4])                                                         = new _Fuse4(o1, o2, o3, o4)
      def fuse[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT]                           (o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3], o4: Fuse[O4], o5: Fuse[O5])                                           = new _Fuse5(o1, o2, o3, o4, o5)
      def fuse[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT]                  (o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3], o4: Fuse[O4], o5: Fuse[O5], o6: Fuse[O6])                             = new _Fuse6(o1, o2, o3, o4, o5, o6)
      def fuse[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT]         (o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3], o4: Fuse[O4], o5: Fuse[O5], o6: Fuse[O6], o7: Fuse[O7])               = new _Fuse7(o1, o2, o3, o4, o5, o6, o7)
      def fuse[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT](o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3], o4: Fuse[O4], o5: Fuse[O5], o6: Fuse[O6], o7: Fuse[O7], o8: Fuse[O8]) = new _Fuse8(o1, o2, o3, o4, o5, o6, o7, o8)

    // ===========================================================================
    class _Fuse2[O1: WTT, O2: WTT](o1: Fuse[O1], o2: Fuse[O2]) {
          def as(d: KPathW) = new {
            def using[D1: WTT](f: (O1, O2) => D1)
              : Self2 = self2 :+ Fuse2(resolve2(o1, o2), ttqkpath1[D1](d), wrap21(f)) } }

      // ---------------------------------------------------------------------------
      class _Fuse2WV(o1: KPath,o2: KPath) {
          def as(d: KPathW) = new {
            def using        (f: (WV, WV) =>     WV)                      : Self2 = self2 :+ Fuse2WV(TargetQueryUtils.tqkpath2(o1, o2), Left(d.value), false, wwwrap21a(f))
            def using        (f: (WV, WV) => Seq[WV])(implicit di: DI)    : Self2 = self2 :+ Fuse2WV(TargetQueryUtils.tqkpath2(o1, o2), Left(d.value), true , wwwrap21a(f) )
            def using[D: WTT](f: (WV, WV) =>     TWV[D])                  : Self2 = self2 :+ Fuse2WV(TargetQueryUtils.tqkpath2(o1, o2), Right(ttqkpath1[D](d)), false, wwwrap21b(f) )
            def using[D: WTT](f: (WV, WV) => Seq[TWV[D]])(implicit di: DI): Self2 = self2 :+ Fuse2WV(TargetQueryUtils.tqkpath2(o1, o2), Right(ttqkpath1[D](d)), true , wwwrap21b(f) )
      } }

    // ---------------------------------------------------------------------------
    class _Fuse3[O1: WTT, O2: WTT, O3: WTT](
      o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3) => D1): Self2 = self2 :+
            Fuse3(resolve3(o1, o2, o3), ttqkpath1[D1](d), wrap31(f)) } }

    // ---------------------------------------------------------------------------
    class _Fuse4[O1: WTT, O2: WTT, O3: WTT, O4: WTT](
      o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3], o4: Fuse[O4]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4) => D1): Self2 = self2 :+
            Fuse4(resolve4(o1, o2, o3, o4), ttqkpath1[D1](d), wrap41(f)) } }


    // ---------------------------------------------------------------------------
    class _Fuse5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](
      o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3], o4: Fuse[O4], o5: Fuse[O5]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4, O5) => D1): Self2 = self2 :+
            Fuse5(resolve5(o1, o2, o3, o4, o5), ttqkpath1[D1](d), wrap51(f)) } }


    // ---------------------------------------------------------------------------
    class _Fuse6[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT](
      o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3], o4: Fuse[O4], o5: Fuse[O5], o6: Fuse[O6]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6) => D1): Self2 = self2 :+
            Fuse6(resolve6(o1, o2, o3, o4, o5, o6), ttqkpath1[D1](d), wrap61(f)) } }


    // ---------------------------------------------------------------------------
    class _Fuse7[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT](
      o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3], o4: Fuse[O4], o5: Fuse[O5], o6: Fuse[O6], o7: Fuse[O7]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7) => D1): Self2 = self2 :+
            Fuse7(resolve7(o1, o2, o3, o4, o5, o6, o7), ttqkpath1[D1](d), wrap71(f)) } }

    // ---------------------------------------------------------------------------
    class _Fuse8[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT](
      o1: Fuse[O1], o2: Fuse[O2], o3: Fuse[O3], o4: Fuse[O4], o5: Fuse[O5], o6: Fuse[O6], o7: Fuse[O7], o8: Fuse[O8]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7, O8) => D1): Self2 = self2 :+
            Fuse8(resolve8(o1, o2, o3, o4, o5, o6, o7, o8), ttqkpath1[D1](d), wrap81(f)) } }

}

// ===========================================================================
