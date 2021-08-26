package gallia.heads.common

import gallia._
import gallia.FunctionWrappers._
import gallia.actions.ActionsUUGenerates._
import gallia.target.utils.TypedTargetQueryUtils._

// ===========================================================================
trait HeadCommonGenerates[F <: HeadCommon[F]] { _: HeadCommon[F] =>
  import TSL.Generate1.{resolve, tqkpath}
  import TSL.Generate2.{resolve => resolve1, _}  

  import TSL.Generate1.{resolve2 => resolve12, resolve3 => resolve13}
  
  // ===========================================================================
  def generateCount(from: RenW, as: KeyW = _count): Self2 = ???//TODO

  // ===========================================================================
  // generate

  def generate(newKey: KPathW) = new _Generate1(newKey.value)

    // ---------------------------------------------------------------------------
    class _Generate1(newPath: KPath)  {

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
          def using         (f: HeadU => HeadU)                            : Self2 = self2 :+ GenerateUU(tqkpath(f1), newPath, wrapUU(f))
          def using         (f: HeadU => HeadZ)    (implicit d: DI)        : Self2 = self2 :+ ??? /* TODO: t210202164324 */
          def using[V1: WTT](f: HeadU => HeadV[V1])(implicit d: DI, d2: DI): Self2 = self2 :+ ??? /* TODO: t210202164324 */ }

        // ---------------------------------------------------------------------------
        class _FromZ1(f1: Generate1[HeadZ]) {
          def using         (f: HeadZ => HeadZ)    (implicit d: DI)        : Self2 = self2 :+ GenerateZZ(tqkpath(f1), newPath, wrapZZ(f))
          def using         (f: HeadZ => HeadU)                            : Self2 = self2 :+ ??? /* TODO: t210202164324 */
          def using[V1: WTT](f: HeadZ => HeadV[V1])(implicit d: DI, d2: DI): Self2 = self2 :+ ??? /* TODO: t210202164324 */ }

        // ---------------------------------------------------------------------------
        class _FromWhatever(f1: Generate1[WV]) {
            private def wrap[T](f: WV => T) = (x: Any) => f(new WV(x)) // TODO: t210816120207 - should this also abstract multiplicity? 
        	  
            // ---------------------------------------------------------------------------
            def using(f: WV => WV): Self2 = self2 :+
              GenerateWV1a(resolve(f1).tqkpath, newPath,               wrap(f)(_).any)

            def using[D: WTT](f: WV => TWV[D]): Self2 = self2 :+
              GenerateWV1b(resolve(f1).tqkpath, tkpath[D](newPath), wrap(f)(_).typed)

            def using[D: WTT](f: WV => D)(implicit di: DI): Self2 = self2 :+
              GenerateWV1b(resolve(f1).tqkpath, tkpath[D](newPath), wrap(f)(_)) }

        // ---------------------------------------------------------------------------
        class _FromV1[O: WTT](f1: Generate1[O]) {
          def using         (f: O => HeadU)                        : Self2 = self2 :+ ???
          def using         (f: O => HeadZ)(implicit d: DI)        : Self2 = self2 :+ ???
          def using[D1: WTT](f: O => D1)   (implicit d: DI, d2: DI): Self2 = self2 :+ Generate1VtoV(resolve(f1), tkpath[D1](newPath), wrap(f)) }

        // ===========================================================================
        //TODO: t210111171545 - validate origins aren't u or z
        class Generate1From2[O1: WTT, O2: WTT](d1: Generate2[O1], d2: Generate2[O2]){
            def using[D1: WTT](f: (O1, O2) => D1): Self2 = self2 :+
              Generate2VtoV(resolve2(d1, d2), tkpath[D1](newPath), wrap21(f)) }

          // ---------------------------------------------------------------------------
          class _FromWhatever2(f1: Generate1[WV], f2: Generate1[WV]) {
        	  private def wrap[T](f: (WV, WV) => T) = (x: Any, y: Any) => f(new WV(x), new WV(y))            

            // ---------------------------------------------------------------------------
            def using(f: (WV, WV) => WV): Self2 = self2 :+
              GenerateWV2a(resolve12(f1, f2).tqkpath2, newPath,               wrap(f)(_, _).any)
          
            def using[D: WTT](f: (WV, WV) => TWV[D]): Self2 = self2 :+
              GenerateWV2b(resolve12(f1, f2).tqkpath2, ttqkpath1[D](newPath), wrap(f)(_, _).typed)
              
            def using[D: WTT](f: (WV, WV) => D)(implicit di: DI): Self2 = self2 :+
              GenerateWV2b(resolve12(f1, f2).tqkpath2, ttqkpath1[D](newPath), wrap(f)(_, _)) }

        // ---------------------------------------------------------------------------
        class Generate1From3[O1: WTT, O2: WTT, O3: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3]) {
            def using[D1: WTT](f: (O1, O2, O3) => D1): Self2 = self2 :+
              Generate3VtoV(resolve3(d1, d2, d3), tkpath[D1](newPath), wrap31(f)) }

          // ---------------------------------------------------------------------------
          class _FromWhatever3(f1: Generate1[WV], f2: Generate1[WV], f3: Generate1[WV]) {
        	  private def wrap[T](f: (WV, WV, WV) => T) = (x: Any, y: Any, z: Any) => f(new WV(x), new WV(y), new WV(z))            

            def using(f: (WV, WV, WV) => WV): Self2 = self2 :+
              GenerateWV3a(resolve13(f1, f2, f3).tqkpath3, newPath,               wrap(f)(_, _, _).any)
          
            def using[D: WTT](f: (WV, WV, WV) => TWV[D]): Self2 = self2 :+
              GenerateWV3b(resolve13(f1, f2, f3).tqkpath3, ttqkpath1[D](newPath), wrap(f)(_, _, _).typed)
              
            def using[D: WTT](f: (WV, WV, WV) => D)(implicit di: DI): Self2 = self2 :+
              GenerateWV3b(resolve13(f1, f2, f3).tqkpath3, ttqkpath1[D](newPath), wrap(f)(_, _, _)) }
        
        // ---------------------------------------------------------------------------
        class Generate1From4[O1: WTT, O2: WTT, O3: WTT, O4: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4]) {
            def using[D1: WTT](f: (O1, O2, O3, O4) => D1): Self2 = self2 :+
              Generate4VtoV(resolve4(d1, d2, d3, d4), tkpath[D1](newPath), wrap41(f)) }

        // ---------------------------------------------------------------------------
        class Generate1From5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5]) {
            def using[D1: WTT](f: (O1, O2, O3, O4, O5) => D1): Self2 = self2 :+
              Generate5VtoV(resolve5(d1, d2, d3, d4, d5), tkpath[D1](newPath), wrap51(f)) }

        // ---------------------------------------------------------------------------
        class Generate1From6[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6]) {
            def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6) => D1): Self2 = self2 :+
              Generate6VtoV(resolve6(d1, d2, d3, d4, d5, d6), tkpath[D1](newPath), wrap61(f)) }

        // ---------------------------------------------------------------------------
        class Generate1From7[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6], d7: Generate2[O7]) {
            def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7) => D1): Self2 = self2 :+
              Generate7VtoV(resolve7(d1, d2, d3, d4, d5, d6, d7), tkpath[D1](newPath), wrap71(f)) }

        // ---------------------------------------------------------------------------
        class Generate1From8[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6], d7: Generate2[O7], d8: Generate2[O8]) {
            def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7, O8) => D1): Self2 = self2 :+
              Generate8VtoV(resolve8(d1, d2, d3, d4, d5, d6, d7, d8), tkpath[D1](newPath), wrap81(f)) }
        
        // ---------------------------------------------------------------------------
        class Generate1From9[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT, O9: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6], d7: Generate2[O7], d8: Generate2[O8], d9: Generate2[O9]) {
            def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7, O8, O9) => D1): Self2 = self2 :+
              Generate9VtoV(resolve9(d1, d2, d3, d4, d5, d6, d7, d8, d9), tkpath[D1](newPath), wrap91(f)) }        

        // ---------------------------------------------------------------------------
        class Generate1From10[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT, O9: WTT, O10: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6], d7: Generate2[O7], d8: Generate2[O8], d9: Generate2[O9], d10: Generate2[O10]) {
            def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10) => D1): Self2 = self2 :+
              Generate10VtoV(resolve10(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10), tkpath[D1](newPath), wrapA1(f)) }        
    }

    // ===========================================================================
    //TODO: t210111171545 - validate origin isn't u or z
    def generate(d1: KPathW, d2: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT](f: O1 => (D1, D2)): Self2 = self2 :+
            GenerateVto2V(resolve1(o1), tkpaths2[D1, D2](d1, d2), wrap12(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT](f: O1 => (D1, D2, D3)): Self2 = self2 :+
            GenerateVto3V(resolve1(o1), tkpaths3[D1, D2, D3](d1, d2, d3), wrap13(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT](f: O1 => (D1, D2, D3, D4)): Self2 = self2 :+
            GenerateVto4V(resolve1(o1), tkpaths4[D1, D2, D3, D4](d1, d2, d3, d4), wrap14(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT](f: O1 => (D1, D2, D3, D4, D5)): Self2 = self2 :+
            GenerateVto5V(resolve1(o1), tkpaths5[D1, D2, D3, D4, D5](d1, d2, d3, d4, d5), wrap15(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT](f: O1 => (D1, D2, D3, D4, D5, D6)): Self2 = self2 :+
            GenerateVto6V(resolve1(o1), tkpaths6[D1, D2, D3, D4, D5, D6](d1, d2, d3, d4, d5, d6), wrap16(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7)): Self2 = self2 :+
            GenerateVto7V(resolve1(o1), tkpaths7[D1, D2, D3, D4, D5, D6, D7](d1, d2, d3, d4, d5, d6, d7), wrap17(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8)): Self2 = self2 :+
            GenerateVto8V(resolve1(o1), tkpaths8[D1, D2, D3, D4, D5, D6, D7, D8](d1, d2, d3, d4, d5, d6, d7, d8), wrap18(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW, d9: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT, D9: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8, D9)): Self2 = self2 :+
            GenerateVto9V(resolve1(o1), tkpaths9[D1, D2, D3, D4, D5, D6, D7, D8, D9](d1, d2, d3, d4, d5, d6, d7, d8, d9), wrap19(f)) } }
      
      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW, d9: KPathW, d10: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT, D9: WTT, D10: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8, D9, D10)): Self2 = self2 :+
            GenerateVto10V(resolve1(o1), tkpaths10[D1, D2, D3, D4, D5, D6, D7, D8, D9, D10](d1, d2, d3, d4, d5, d6, d7, d8, d9, d10), wrap1A(f)) } }
}

// ===========================================================================
