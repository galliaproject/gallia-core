package gallia
package heads.common

import FunctionWrappers._
import actions.ActionsUUFusion._

// ===========================================================================
trait HeadCommonFusion[F <: HeadCommon[F]] { ignored: HeadCommon[F] =>
  import TSL.FuseFission._

  // ---------------------------------------------------------------------------
  def fuse(o1: KPathW, o2: KPathW)             = new _Whatever2(_._explicit(o1.value), _._explicit(o2.value))
  def fuse(o1: KPathW, o2: KPathW, o3: KPathW) = new _Whatever3(_._explicit(o1.value), _._explicit(o2.value), _._explicit(o3.value))  
  
  // ---------------------------------------------------------------------------
  @Max5
  def fuse[O1: WTT, O2: WTT](o1: Fusion[O1], o2: Fusion[O2]) = new _Fuse2(o1, o2)

      def fuse[O1: WTT, O2: WTT, O3: WTT]                                             (o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3])                                                                                 = new _Fuse3(o1, o2, o3)
      def fuse[O1: WTT, O2: WTT, O3: WTT, O4: WTT]                                    (o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4])                                                                 = new _Fuse4(o1, o2, o3, o4)
      def fuse[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT]                           (o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4], o5: Fusion[O5])                                                 = new _Fuse5(o1, o2, o3, o4, o5)
      def fuse[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT]                  (o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4], o5: Fusion[O5], o6: Fusion[O6])                                 = new _Fuse6(o1, o2, o3, o4, o5, o6)
      def fuse[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT]         (o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4], o5: Fusion[O5], o6: Fusion[O6], o7: Fusion[O7])                 = new _Fuse7(o1, o2, o3, o4, o5, o6, o7)
      def fuse[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT](o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4], o5: Fusion[O5], o6: Fusion[O6], o7: Fusion[O7], o8: Fusion[O8]) = new _Fuse8(o1, o2, o3, o4, o5, o6, o7, o8)

    // ===========================================================================
    class _Fuse2[O1: WTT, O2: WTT](o1: Fusion[O1], o2: Fusion[O2]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2) => D1): Self2 = self2 :+ 
            Fuse2(resolve2(o1, o2), tkpath[D1](d), wrap21(f)) } }
  
    // ---------------------------------------------------------------------------
    class _Fuse3[O1: WTT, O2: WTT, O3: WTT](
      o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3) => D1): Self2 = self2 :+
            Fuse3(resolve3(o1, o2, o3), tkpath[D1](d), wrap31(f)) } }

    // ---------------------------------------------------------------------------
    class _Fuse4[O1: WTT, O2: WTT, O3: WTT, O4: WTT](
      o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4) => D1): Self2 = self2 :+
            Fuse4(resolve4(o1, o2, o3, o4), tkpath[D1](d), wrap41(f)) } }

    // ---------------------------------------------------------------------------
    class _Fuse5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](
      o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4], o5: Fusion[O5]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4, O5) => D1): Self2 = self2 :+
            Fuse5(resolve5(o1, o2, o3, o4, o5), tkpath[D1](d), wrap51(f)) } }

    // ---------------------------------------------------------------------------
    class _Fuse6[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT](
      o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4], o5: Fusion[O5], o6: Fusion[O6]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6) => D1): Self2 = self2 :+
            Fuse6(resolve6(o1, o2, o3, o4, o5, o6), tkpath[D1](d), wrap61(f)) } }

    // ---------------------------------------------------------------------------
    class _Fuse7[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT](
      o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4], o5: Fusion[O5], o6: Fusion[O6], o7: Fusion[O7]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7) => D1): Self2 = self2 :+
            Fuse7(resolve7(o1, o2, o3, o4, o5, o6, o7), tkpath[D1](d), wrap71(f)) } }

    // ---------------------------------------------------------------------------
    class _Fuse8[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT](
      o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4], o5: Fusion[O5], o6: Fusion[O6], o7: Fusion[O7], o8: Fusion[O8]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7, O8) => D1): Self2 = self2 :+
            Fuse8(resolve8(o1, o2, o3, o4, o5, o6, o7, o8), tkpath[D1](d), wrap81(f)) } }

    // ---------------------------------------------------------------------------
    class _Fuse9[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT, O9: WTT](
      o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4], o5: Fusion[O5], o6: Fusion[O6], o7: Fusion[O7], o8: Fusion[O8], o9: Fusion[O9]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7, O8, O9) => D1): Self2 = self2 :+
            Fuse9(resolve9(o1, o2, o3, o4, o5, o6, o7, o8, o9), tkpath[D1](d), wrap91(f)) } }
    
    // ---------------------------------------------------------------------------
    class _Fuse10[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT, O9: WTT, O10: WTT](
      o1: Fusion[O1], o2: Fusion[O2], o3: Fusion[O3], o4: Fusion[O4], o5: Fusion[O5], o6: Fusion[O6], o7: Fusion[O7], o8: Fusion[O8], o9: Fusion[O9], o10: Fusion[O10]) {
        def as(d: KPathW) = new {
          def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10) => D1): Self2 = self2 :+
            Fuse10(resolve10(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10), tkpath[D1](d), wrapA1(f)) } }
    
  // ===========================================================================  
  class _Whatever2(o1: Fusion[WV], o2: Fusion[WV]) {
      def as(d: KPathW) = new {
    	  // TODO: t210816120207 - should this also abstract multiplicity?
        private def wrap[T, U](f: (WV, WV) => T)(g: T => U) = (x: Any, y: Any) => g(f(new WV(x), new WV(y)))
        
        // ---------------------------------------------------------------------------
        def using        (f: (WV, WV) => WV): Self2 = self2 :+ 
          FusionWV2a(resolve2(o1, o2).tqkpath2, d.value, wrap(f)(_.any))
         
        def using[D: WTT](f: (WV, WV) => TWV[D]): Self2 = self2 :+          
          FusionWV2b(resolve2(o1, o2).tqkpath2, d.value, typeNode[D], wrap(f)(_.typed))      

        def using[D: WTT](f: (WV, WV) => D)(implicit di: DI): Self2 = self2 :+
          FusionWV2b(resolve2(o1, o2).tqkpath2, d.value, typeNode[D], wrap(f)(x => x)) } }
    
    // ---------------------------------------------------------------------------  
    class _Whatever3(o1: Fusion[WV], o2: Fusion[WV], o3: Fusion[WV]) {
      def as(d: KPathW) = new {
    	  // TODO: t210816120207 - should this also abstract multiplicity?
        private def wrap[T, U](f: (WV, WV, WV) => T)(g: T => U) = (x: Any, y: Any, z: Any) => g(f(new WV(x), new WV(y), new WV(z)))
        
        // ---------------------------------------------------------------------------
        def using        (f: (WV, WV, WV) => WV): Self2 = self2 :+ 
          FusionWV3a(resolve3(o1, o2, o3).tqkpath3, d.value, wrap(f)(_.any))
         
        def using[D: WTT](f: (WV, WV, WV) => TWV[D]): Self2 = self2 :+          
          FusionWV3b(resolve3(o1, o2, o3).tqkpath3, d.value, typeNode[D], wrap(f)(_.typed))      

        def using[D: WTT](f: (WV, WV, WV) => D)(implicit di: DI): Self2 = self2 :+
          FusionWV3b(resolve3(o1, o2, o3).tqkpath3, d.value, typeNode[D], wrap(f)(x => x)) } }

}

// ===========================================================================
