package gallia
package heads.common

import FunctionWrappers._
import actions.common.ActionsCommonFission._

// ===========================================================================
trait HeadCommonFission[F <: HeadCommon[F]] { ignored: HeadCommon[F] =>
  import TSL.FuseFission._ 

  // ===========================================================================
  def fission(o: KPathW): _FromWhatever = new _FromWhatever(_._explicit(o.value))

    @Max3
    class _FromWhatever(o: Fission[WV]) {

      // ---------------------------------------------------------------------------
      def as(d1: KPathW, d2: KPathW) = new {
    	  // TODO: t210816120207 - should this also abstract multiplicity?
        private def wrap[T, U](f: WV => (T, T))(g: T => U) = (x: Any) => { val (y1, y2) = f(new WV(x)); (g(y1), g(y2)) }
        
        // ---------------------------------------------------------------------------
        def using                  (f: WV => (WV, WV)): Self2 = self2 :+ 
          FissionWV2a(resolve(o).tqkpath, kpaths2(d1, d2),           wrap(f)(_.any))

        def using[D1: WTT, D2: WTT](f: WV => (TWV[D1], TWV[D2])): Self2 = self2 :+          
          FissionWV2b(resolve(o).tqkpath, tkpaths2[D1, D2](d1, d2), wrap(f)(_.typed))      

        def using[D1: WTT, D2: WTT](f: WV => (D1, D2))(implicit di: DI): Self2 = self2 :+
          FissionWV2b(resolve(o).tqkpath, tkpaths2[D1, D2](d1, d2), wrap(f)(x => x)) }

      // ---------------------------------------------------------------------------  
      def as(d1: KPathW, d2: KPathW, d3: KPathW) = new {
    	  // TODO: t210816120207 - should this also abstract multiplicity?
        private def wrap[T, U](f: WV => (T, T, T))(g: T => U) = (x: Any) => { val (y1, y2, y3) = f(new WV(x)); (g(y1), g(y2), g(y3)) }

        // ---------------------------------------------------------------------------
        def using                           (f: WV => (WV, WV, WV)): Self2 = self2 :+ 
          FissionWV3a(resolve(o).tqkpath, kpaths3(d1, d2, d3), wrap(f)(_.any))
          
        def using[D1: WTT, D2: WTT, D3: WTT](f: WV => (TWV[D1], TWV[D2], TWV[D3])): Self2 = self2 :+          
          FissionWV3b(resolve(o).tqkpath, tkpaths3[D1, D2, D3](d1, d2, d3), wrap(f)(_.typed))      

        def using[D1: WTT, D2: WTT, D3: WTT](f: WV => (D1, D2, D3))(implicit di: DI): Self2 = self2 :+
          FissionWV3b(resolve(o).tqkpath, tkpaths3[D1, D2, D3](d1, d2, d3), wrap(f)(x => x)) }
  }

  // ===========================================================================
  @Max10
  def fission[O1: WTT](o1: Fission[O1]) = new {

    def as(d1: KPathW, d2: KPathW) = new {
      def using[D1: WTT, D2: WTT](f: O1 => (D1, D2)): Self2 = self2 :+ 
          Fission2(resolve(o1), tkpaths2[D1, D2](d1, d2), wrap12(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT](f: O1 => (D1, D2, D3)): Self2 = self2 :+ 
          Fission3(resolve(o1), tkpaths3[D1, D2, D3](d1, d2, d3), wrap13(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT](f: O1 => (D1, D2, D3, D4)): Self2 = self2 :+
          Fission4(resolve(o1), tkpaths4[D1, D2, D3, D4](d1, d2, d3, d4), wrap14(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT](f: O1 => (D1, D2, D3, D4, D5)): Self2 = self2 :+
          Fission5(resolve(o1), tkpaths5[D1, D2, D3, D4, D5](d1, d2, d3, d4, d5), wrap15(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT](f: O1 => (D1, D2, D3, D4, D5, D6)): Self2 = self2 :+
          Fission6(resolve(o1), tkpaths6[D1, D2, D3, D4, D5, D6](d1, d2, d3, d4, d5, d6), wrap16(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7)): Self2 = self2 :+
          Fission7(resolve(o1), tkpaths7[D1, D2, D3, D4, D5, D6, D7](d1, d2, d3, d4, d5, d6, d7), wrap17(f)) }

    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8)): Self2 = self2 :+
          Fission8(resolve(o1), tkpaths8[D1, D2, D3, D4, D5, D6, D7, D8](d1, d2, d3, d4, d5, d6, d7, d8), wrap18(f)) }
    
    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW, d9: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT, D9: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8, D9)): Self2 = self2 :+
          Fission9(resolve(o1), tkpaths9[D1, D2, D3, D4, D5, D6, D7, D8, D9](d1, d2, d3, d4, d5, d6, d7, d8, d9), wrap19(f)) }
        
    // ---------------------------------------------------------------------------
    def as(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW, d9: KPathW, d10: KPathW) = new {
      def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT, D9: WTT, D10: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8, D9, D10)): Self2 = self2 :+
          Fission10(resolve(o1), tkpaths10[D1, D2, D3, D4, D5, D6, D7, D8, D9, D10](d1, d2, d3, d4, d5, d6, d7, d8, d9, d10), wrap1A(f)) }

  }

}

// ===========================================================================
