package gallia
package heads.common

import FunctionWrappers._
import target.utils.TypedTargetQueryUtils._
import actions.ActionsUUGenerationOther
import actions.ActionsUUGenerusion._

// ===========================================================================
trait HeadCommonGenerusion[F <: HeadCommon[F]] { _: HeadCommon[F] =>
  import TSL.Generate1.{resolve, tqkpath, resolve2 => resolve2w, resolve3 => resolve3w}
  import TSL.Generate2._

  // ---------------------------------------------------------------------------
  def generate(d: KPathW) = new _Generusion(d.value)

  // ===========================================================================
  class _Generusion(d: KPath)  {

    def from        (f1: KPathW)                                    : _FromWhatever = new _FromWhatever(_._explicit(f1.value))
    def from        (f1: Generate1[HeadU])                          : _FromU1       = new _FromU1(f1)
    def from        (f1: Generate1[HeadZ])(implicit di: DI)         : _FromZ1       = new _FromZ1(f1)
    def from[O: WTT](f1: Generate1[O])    (implicit di: DI, di2: DI): _FromV1[O]    = new _FromV1(f1)

    // ---------------------------------------------------------------------------
    def from                  (f1: KPathW, f2: KPathW)               = new _FromWhatever2(_._explicit(f1.value), _._explicit(f2.value))
    def from[O1: WTT, O2: WTT](f1: Generate2[O1], f2: Generate2[O2]) = new Generate1From2(f1, f2)
    
    def from                           (f1: KPathW, f2: KPathW, f3: KPathW)                      = new _FromWhatever3(_._explicit(f1.value), _._explicit(f2.value), _._explicit(f3.value))
    def from[O1: WTT, O2: WTT, O3: WTT](f1: Generate2[O1], f2: Generate2[O2], f3: Generate2[O3]) = new Generate1From3(f1, f2, f3)

    // ---------------------------------------------------------------------------
    def from[O1: WTT, O2: WTT, O3: WTT, O4: WTT]                                    (f1: Generate2[O1], f2: Generate2[O2], f3: Generate2[O3], f4: Generate2[O4])                                                                             = new Generate1From4(f1, f2, f3, f4)
    def from[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT]                           (f1: Generate2[O1], f2: Generate2[O2], f3: Generate2[O3], f4: Generate2[O4], f5: Generate2[O5])                                                          = new Generate1From5(f1, f2, f3, f4, f5)
    def from[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT]                  (f1: Generate2[O1], f2: Generate2[O2], f3: Generate2[O3], f4: Generate2[O4], f5: Generate2[O5], f6: Generate2[O6])                                       = new Generate1From6(f1, f2, f3, f4, f5, f6)
    def from[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT]         (f1: Generate2[O1], f2: Generate2[O2], f3: Generate2[O3], f4: Generate2[O4], f5: Generate2[O5], f6: Generate2[O6], f7: Generate2[O7])                    = new Generate1From7(f1, f2, f3, f4, f5, f6, f7)
    def from[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT](f1: Generate2[O1], f2: Generate2[O2], f3: Generate2[O3], f4: Generate2[O4], f5: Generate2[O5], f6: Generate2[O6], f7: Generate2[O7], f8: Generate2[O8]) = new Generate1From8(f1, f2, f3, f4, f5, f6, f7, f8)

      // ===========================================================================
      class _FromU1(f1: Generate1[HeadU]) {
        def using         (f: HeadU => HeadU)                             : Self2 = self2 :+ ActionsUUGenerationOther.GenerateUU(tqkpath(f1), d, wrapUU(f))
        def using         (f: HeadU => HeadZ)    (implicit d1: DI)        : Self2 = self2 :+ ??? /* TODO: t210202164324 */
        def using[V1: WTT](f: HeadU => HeadV[V1])(implicit d1: DI, d2: DI): Self2 = self2 :+ ??? /* TODO: t210202164324 */ }

      // ---------------------------------------------------------------------------
      class _FromZ1(f1: Generate1[HeadZ]) {
        def using         (f: HeadZ => HeadZ)    (implicit d1: DI)        : Self2 = self2 :+ ActionsUUGenerationOther.GenerateZZ(tqkpath(f1), d, wrapZZ(f))
        def using         (f: HeadZ => HeadU)                             : Self2 = self2 :+ ??? /* TODO: t210202164324 */
        def using[V1: WTT](f: HeadZ => HeadV[V1])(implicit d1: DI, d2: DI): Self2 = self2 :+ ??? /* TODO: t210202164324 */ }

      // ---------------------------------------------------------------------------
      class _FromWhatever(f1: Generate1[WV]) {
          private def wrap[T](f: WV => T) = (x: Any) => f(new WV(x)) // TODO: t210816120207 - should this also abstract multiplicity? 
      	  
          // ---------------------------------------------------------------------------
          def using(f: WV => WV): Self2 = self2 :+
            GenerateWV1a(resolve(f1).tqkpath, d,               wrap(f)(_).any)

          def using[D: WTT](f: WV => TWV[D]): Self2 = self2 :+
            GenerateWV1b(resolve(f1).tqkpath, tkpath[D](d), wrap(f)(_).typed)

          def using[D: WTT](f: WV => D)(implicit di: DI): Self2 = self2 :+
            GenerateWV1b(resolve(f1).tqkpath, tkpath[D](d), wrap(f)(_)) }

      // ---------------------------------------------------------------------------
      class _FromV1[O: WTT](f1: Generate1[O]) {
        def using        (f: O => HeadU)                         : Self2 = self2 :+ ???
        def using        (f: O => HeadZ)(implicit d1: DI)        : Self2 = self2 :+ ???
        def using[D: WTT](f: O => D)    (implicit d1: DI, d2: DI): Self2 = self2 :+ ActionsUUGenerationOther.GenerateVtoV(resolve(f1), tkpath[D](d), wrap(f)) }

      // ===========================================================================
      class _FromWhatever2(f1: Generate1[WV], f2: Generate1[WV]) {
      	  private def wrap[T](f: (WV, WV) => T) = (x: Any, y: Any) => f(new WV(x), new WV(y))            

          // ---------------------------------------------------------------------------
          def using(f: (WV, WV) => WV): Self2 = self2 :+
            GenerateWV2a(resolve2w(f1, f2).tqkpath2, d,               wrap(f)(_, _).any)
        
          def using[D: WTT](f: (WV, WV) => TWV[D]): Self2 = self2 :+
            GenerateWV2b(resolve2w(f1, f2).tqkpath2, ttqkpath1[D](d), wrap(f)(_, _).typed)
            
          def using[D: WTT](f: (WV, WV) => D)(implicit di: DI): Self2 = self2 :+
            GenerateWV2b(resolve2w(f1, f2).tqkpath2, ttqkpath1[D](d), wrap(f)(_, _)) }

        // ---------------------------------------------------------------------------
        class _FromWhatever3(f1: Generate1[WV], f2: Generate1[WV], f3: Generate1[WV]) {
      	  private def wrap[T](f: (WV, WV, WV) => T) = (x: Any, y: Any, z: Any) => f(new WV(x), new WV(y), new WV(z))            

          def using(f: (WV, WV, WV) => WV): Self2 = self2 :+
            GenerateWV3a(resolve3w(f1, f2, f3).tqkpath3, d,               wrap(f)(_, _, _).any)
        
          def using[D: WTT](f: (WV, WV, WV) => TWV[D]): Self2 = self2 :+
            GenerateWV3b(resolve3w(f1, f2, f3).tqkpath3, ttqkpath1[D](d), wrap(f)(_, _, _).typed)
            
          def using[D: WTT](f: (WV, WV, WV) => D)(implicit di: DI): Self2 = self2 :+
            GenerateWV3b(resolve3w(f1, f2, f3).tqkpath3, ttqkpath1[D](d), wrap(f)(_, _, _)) }
        
      // ===========================================================================
      //TODO: t210111171545 - validate origins aren't u or z
      class Generate1From2[O1: WTT, O2: WTT](d1: Generate2[O1], d2: Generate2[O2]){
          def using[D: WTT](f: (O1, O2) => D): Self2 = self2 :+
            Generate2VtoV(resolve2(d1, d2), tkpath[D](d), wrap21(f)) }
        
      // ---------------------------------------------------------------------------
      class Generate1From3[O1: WTT, O2: WTT, O3: WTT](
        d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3]) {
          def using[D: WTT](f: (O1, O2, O3) => D): Self2 = self2 :+
            Generate3VtoV(resolve3(d1, d2, d3), tkpath[D](d), wrap31(f)) }
      
      // ---------------------------------------------------------------------------
      class Generate1From4[O1: WTT, O2: WTT, O3: WTT, O4: WTT](
        d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4]) {
          def using[D: WTT](f: (O1, O2, O3, O4) => D): Self2 = self2 :+
            Generate4VtoV(resolve4(d1, d2, d3, d4), tkpath[D](d), wrap41(f)) }

      // ---------------------------------------------------------------------------
      class Generate1From5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](
        d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5]) {
          def using[D: WTT](f: (O1, O2, O3, O4, O5) => D): Self2 = self2 :+
            Generate5VtoV(resolve5(d1, d2, d3, d4, d5), tkpath[D](d), wrap51(f)) }

      // ---------------------------------------------------------------------------
      class Generate1From6[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT](
        d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6]) {
          def using[D: WTT](f: (O1, O2, O3, O4, O5, O6) => D): Self2 = self2 :+
            Generate6VtoV(resolve6(d1, d2, d3, d4, d5, d6), tkpath[D](d), wrap61(f)) }

      // ---------------------------------------------------------------------------
      class Generate1From7[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT](
        d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6], d7: Generate2[O7]) {
          def using[D: WTT](f: (O1, O2, O3, O4, O5, O6, O7) => D): Self2 = self2 :+
            Generate7VtoV(resolve7(d1, d2, d3, d4, d5, d6, d7), tkpath[D](d), wrap71(f)) }

      // ---------------------------------------------------------------------------
      class Generate1From8[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT](
        d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6], d7: Generate2[O7], d8: Generate2[O8]) {
          def using[D: WTT](f: (O1, O2, O3, O4, O5, O6, O7, O8) => D): Self2 = self2 :+
            Generate8VtoV(resolve8(d1, d2, d3, d4, d5, d6, d7, d8), tkpath[D](d), wrap81(f)) }
      
      // ---------------------------------------------------------------------------
      class Generate1From9[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT, O9: WTT](
        d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6], d7: Generate2[O7], d8: Generate2[O8], d9: Generate2[O9]) {
          def using[D: WTT](f: (O1, O2, O3, O4, O5, O6, O7, O8, O9) => D): Self2 = self2 :+
            Generate9VtoV(resolve9(d1, d2, d3, d4, d5, d6, d7, d8, d9), tkpath[D](d), wrap91(f)) }        

      // ---------------------------------------------------------------------------
      class Generate1From10[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT, O9: WTT, O10: WTT](
        d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6], d7: Generate2[O7], d8: Generate2[O8], d9: Generate2[O9], d10: Generate2[O10]) {
          def using[D: WTT](f: (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10) => D): Self2 = self2 :+
            Generate10VtoV(resolve10(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10), tkpath[D](d), wrapA1(f)) }        
  }

}

// ===========================================================================
