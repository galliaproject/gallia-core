package gallia.heads.common

import gallia._
import gallia.FunctionWrappers._
import gallia.actions.ActionsUUGenerates._
import gallia.target.utils.TypedTargetQueryUtils._

// ===========================================================================
trait HeadCommonGenerates[F <: HeadCommon[F]] { _: HeadCommon[F] =>
  import TSL.Generate1.{resolve, tqkpath}
  import TSL.Generate2.{resolve => resolve1, _}  

  import TSL.Generate1.{resolve2 => resolve12}
  
  // ===========================================================================
  def generateCount(from: RenW, as: KeyW = _count): Self2 = ???//TODO

  // ===========================================================================
  // generate

  def generate(newKey: KPathW) = new _Generate1(newKey.value)

    // ---------------------------------------------------------------------------
    class _Generate1(newPath: KPath)  {

      def from        (f1: Generate1[HeadU])                          : _FromU1    = new _FromU1(f1)
      def from        (f1: Generate1[HeadZ])(implicit di: DI)         : _FromZ1    = new _FromZ1(f1)
      def from[O: WTT](f1: Generate1[O])    (implicit di: DI, di2: DI): _FromV1[O] = new _FromV1(f1)

      // ---------------------------------------------------------------------------
      //TODO
      def from (k: KeyW): _FromWhatever = new _FromWhatever(_._explicit(k.value))

      //def from         (f1: KPathW)      : _From11[WV] = from(_.explicit(f1.value))
      //def from[O1: WTT](f1: Generate[O1]): _From11[O1] = new _From11(f1)

      // ---------------------------------------------------------------------------
      def from(f1: KPathW, f2: KPathW) = new _FromWhatever2(_._explicit(f1.value), _._explicit(f2.value)) // TODO: also add for 3 to 10

      def from[O1: WTT, O2: WTT]                  (f1: Generate2[O1], f2: Generate2[O2])                                       = new Generate1From2(f1, f2)
      def from[O1: WTT, O2: WTT, O3: WTT]         (f1: Generate2[O1], f2: Generate2[O2], f3: Generate2[O3])                    = new Generate1From3(f1, f2, f3)
      def from[O1: WTT, O2: WTT, O3: WTT, O4: WTT](f1: Generate2[O1], f2: Generate2[O2], f3: Generate2[O3], f4: Generate2[O4]) = new Generate1From4(f1, f2, f3, f4)

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
        	  private def wrap[T](f: WV => T) = (x: Any) => f(new WV(x))
        	  
            def using(f: WV => WV): Self2 = self2 :+
              GenerateWV1a(resolve(f1).tqkpath, newPath,               wrap(f)(_).any)

            def using[D: WTT](f: WV => TWV[D]): Self2 = self2 :+
              GenerateWV1b(resolve(f1).tqkpath, ttqkpath1[D](newPath), wrap(f)(_).forceOne)

            def using[D: WTT](f: WV => D)(implicit di: DI): Self2 = self2 :+
              GenerateWV1b(resolve(f1).tqkpath, ttqkpath1[D](newPath), wrap(f)(_)) }

        // ---------------------------------------------------------------------------
        class _FromV1[O: WTT](f1: Generate1[O]) {
          def using         (f: O => HeadU)                        : Self2 = self2 :+ ???
          def using         (f: O => HeadZ)(implicit d: DI)        : Self2 = self2 :+ ???
          def using[D1: WTT](f: O => D1)   (implicit d: DI, d2: DI): Self2 = self2 :+ Generate1VtoV(resolve(f1), ttqkpath1[D1](newPath), wrap(f)) }

        // ===========================================================================
        //TODO: t210111171545 - validate origins aren't u or z
        class Generate1From2[O1: WTT, O2: WTT](d1: Generate2[O1], d2: Generate2[O2]){
          def using[D1: WTT](f: (O1, O2) => D1): Self2 = self2 :+
            {
              val dest = ttqkpath1[D1](newPath)
              Generate2VtoV(resolve2(d1, d2), dest, if (dest.node.isWhatever0) __wwrap21(f) else wrap21(f))
            }
          }
        
          // ---------------------------------------------------------------------------
          class _FromWhatever2(f1: Generate1[WV], f2: Generate1[WV]) {
        	  private def wrap[T](f: (WV, WV) => T) = (x: Any, y: Any) => f(new WV(x), new WV(y))            

            def using(f: (WV, WV) => WV): Self2 = self2 :+
              GenerateWV2a(resolve12(f1, f2).tqkpath2, newPath,               wrap(f)(_, _).any)
          
            def using[D: WTT](f: (WV, WV) => TWV[D]): Self2 = self2 :+
              GenerateWV2b(resolve12(f1, f2).tqkpath2, ttqkpath1[D](newPath), wrap(f)(_, _).forceOne)
              
            def using[D: WTT](f: (WV, WV) => D)(implicit di: DI): Self2 = self2 :+
              GenerateWV2b(resolve12(f1, f2).tqkpath2, ttqkpath1[D](newPath), wrap(f)(_, _)) }

        // ---------------------------------------------------------------------------
        class Generate1From3[O1: WTT, O2: WTT, O3: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3]) {
            def using[D1: WTT](f: (O1, O2, O3) => D1): Self2 = self2 :+
              Generate3VtoV(resolve3(d1, d2, d3), ttqkpath1[D1](newPath), wrap31(f)) }

        // ---------------------------------------------------------------------------
        class Generate1From4[O1: WTT, O2: WTT, O3: WTT, O4: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4]) {
            def using[D1: WTT](f: (O1, O2, O3, O4) => D1): Self2 = self2 :+
              Generate4VtoV(resolve4(d1, d2, d3, d4), ttqkpath1[D1](newPath), wrap41(f)) }

        // ---------------------------------------------------------------------------
        class Generate1From5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5]) {
            def using[D1: WTT](f: (O1, O2, O3, O4, O5) => D1): Self2 = self2 :+
              Generate5VtoV(resolve5(d1, d2, d3, d4, d5), ttqkpath1[D1](newPath), wrap51(f)) }

        // ---------------------------------------------------------------------------
        class Generate1From6[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6]) {
            def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6) => D1): Self2 = self2 :+
              Generate6VtoV(resolve6(d1, d2, d3, d4, d5, d6), ttqkpath1[D1](newPath), wrap61(f)) }

        // ---------------------------------------------------------------------------
        class Generate1From7[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6], d7: Generate2[O7]) {
            def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7) => D1): Self2 = self2 :+
              Generate7VtoV(resolve7(d1, d2, d3, d4, d5, d6, d7), ttqkpath1[D1](newPath), wrap71(f)) }

        // ---------------------------------------------------------------------------
        class Generate1From8[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, O6: WTT, O7: WTT, O8: WTT](
          d1: Generate2[O1], d2: Generate2[O2], d3: Generate2[O3], d4: Generate2[O4], d5: Generate2[O5], d6: Generate2[O6], d7: Generate2[O7], d8: Generate2[O8]) {
            def using[D1: WTT](f: (O1, O2, O3, O4, O5, O6, O7, O8) => D1): Self2 = self2 :+
              Generate8VtoV(resolve8(d1, d2, d3, d4, d5, d6, d7, d8), ttqkpath1[D1](newPath), wrap81(f)) }
    }

    // ===========================================================================
    //TODO: t210111171545 - validate origin isn't u or z
    def generate(d1: KPathW, d2: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT](f: O1 => (D1, D2)): Self2 = self2 :+
            GenerateVto2V(resolve1(o1), ttqkpath2[D1, D2](d1, d2), wrap12(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT](f: O1 => (D1, D2, D3)): Self2 = self2 :+
            GenerateVto3V(resolve1(o1), ttqkpath3[D1, D2, D3](d1, d2, d3), wrap13(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT](f: O1 => (D1, D2, D3, D4)): Self2 = self2 :+
            GenerateVto4V(resolve1(o1), ttqkpath4[D1, D2, D3, D4](d1, d2, d3, d4), wrap14(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT](f: O1 => (D1, D2, D3, D4, D5)): Self2 = self2 :+
            GenerateVto5V(resolve1(o1), ttqkpath5[D1, D2, D3, D4, D5](d1, d2, d3, d4, d5), wrap15(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT](f: O1 => (D1, D2, D3, D4, D5, D6)): Self2 = self2 :+
            GenerateVto6V(resolve1(o1), ttqkpath6[D1, D2, D3, D4, D5, D6](d1, d2, d3, d4, d5, d6), wrap16(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7)): Self2 = self2 :+
            GenerateVto7V(resolve1(o1), ttqkpath7[D1, D2, D3, D4, D5, D6, D7](d1, d2, d3, d4, d5, d6, d7), wrap17(f)) } }

      // ---------------------------------------------------------------------------
      def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW) = new {
        def from[O1: WTT](o1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8)): Self2 = self2 :+
            GenerateVto8V(resolve1(o1), ttqkpath8[D1, D2, D3, D4, D5, D6, D7, D8](d1, d2, d3, d4, d5, d6, d7, d8), wrap18(f)) } }

}

// ===========================================================================
