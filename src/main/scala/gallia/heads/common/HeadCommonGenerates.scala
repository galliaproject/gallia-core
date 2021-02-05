package gallia.heads.common

import gallia._
import gallia.FunctionWrappers._
import gallia.actions.ActionsUUGenerates._
import gallia.target.utils.TypedTargetQueryUtils._

// ===========================================================================
trait HeadCommonGenerates[F <: HeadCommon[F]] { _: HeadCommon[F] =>
  import TSL.Generate1.{resolve, tqkpath}
  import TSL.Generate2.{resolve => resolve1, resolve2, resolve3, resolve4, resolve5}

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
      def from (k: Key): _FromWhatever = new _FromWhatever(_._explicit(k))

      //def from         (f1: KPathW)       : _From11[WV] = from(_.explicit(f1.value))
      //def from[O1: WTT](f1: Generate[O1]): _From11[O1] = new _From11(f1)

      // ---------------------------------------------------------------------------
      def from(f1: KPathW, f2: KPathW) = from[WV, WV](_._explicit(f1), _._explicit(f2))

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
          def using[D: WTT](f: WV => WV2[D]): Self2 = self2 :+
            GenerateWV(resolve(f1).tqkpath, ttqkpath1[D](newPath), (x: Any) => f(new WV(x)).forceOne) }

        // ---------------------------------------------------------------------------
        class _FromV1[O: WTT](f1: Generate1[O]) {
          def using         (f: O => HeadU)                        : Self2 = self2 :+ ???
          def using         (f: O => HeadZ)(implicit d: DI)        : Self2 = self2 :+ ???
          def using[D1: WTT](f: O => D1)   (implicit d: DI, d2: DI): Self2 = self2 :+ Generate1VtoV(resolve(f1), ttqkpath1[D1](newPath), wrap(f)) }

        // ===========================================================================
        //TODO: t210111171545 - validate origins aren't u or z
        class Generate1From2[O1: WTT, O2: WTT](f1: Generate2[O1], f2: Generate2[O2]){
          def using[D1: WTT](f: (O1, O2) => D1): Self2 = self2 :+
            {
              val dest = ttqkpath1[D1](newPath)
              Generate2VtoV(resolve2(f1, f2), dest, if (dest.node.isWhatever0) __wwrap21(f) else wrap21(f))
            }
          }


        // ---------------------------------------------------------------------------
        class Generate1From3[O1: WTT, O2: WTT, O3: WTT](f1: Generate2[O1], f2: Generate2[O2], f3: Generate2[O3]) {
          def using[D1: WTT](f: (O1, O2, O3) => D1): Self2 =  self2 :+
            Generate3VtoV(resolve3(f1, f2, f3), ttqkpath1[D1](newPath), wrap31(f)) }

        // ---------------------------------------------------------------------------
        class Generate1From4[O1: WTT, O2: WTT, O3: WTT, O4: WTT](f1: Generate2[O1], f2: Generate2[O2], f3: Generate2[O3], f4: Generate2[O4]) {
          def using[D1: WTT](f: (O1, O2, O3, O4) => D1): Self2 = self2 :+
            Generate4VtoV(resolve4(f1, f2, f3, f4), ttqkpath1[D1](newPath), wrap41(f)) }

        // ---------------------------------------------------------------------------
        class Generate1From5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](f1: Generate2[O1], f2: Generate2[O2], f3: Generate2[O3], f4: Generate2[O4], f5: Generate2[O5]) {
          def using[D1: WTT](f: (O1, O2, O3, O4, O5) => D1): Self2 = self2 :+
            Generate5VtoV(resolve5(f1, f2, f3, f4, f5), ttqkpath1[D1](newPath), wrap51(f)) }
    }

    // ===========================================================================
    //TODO: t210111171545 - validate origin isn't u or z
    def generate(a: KPathW, b: KPathW) = new {
        def from[O1: WTT](f1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT](f: O1 => (D1, D2)): Self2 = self2 :+
            GenerateVto2V(resolve1(f1), ttqkpath2[D1, D2](a, b), wrap12(f)) } }

      // ---------------------------------------------------------------------------
      def generate(a: KPathW, b: KPathW, c: KPathW) = new {
        def from[O1: WTT](f1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT](f: O1 => (D1, D2, D3)): Self2 = self2 :+
            GenerateVto3V(resolve1(f1), ttqkpath3[D1, D2, D3](a, b, c), wrap13(f)) } }

      // ---------------------------------------------------------------------------
      def generate(a: KPathW, b: KPathW, c: KPathW, d: KPathW) = new {
        def from[O1: WTT](f1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT](f: O1 => (D1, D2, D3, D4)): Self2 = self2 :+
            GenerateVto4V(resolve1(f1), ttqkpath4[D1, D2, D3, D4](a, b, c, d), wrap14(f)) } }

      // ---------------------------------------------------------------------------
      def generate(a: KPathW, b: KPathW, c: KPathW, d: KPathW, e: KPathW) = new {
        def from[O1: WTT](f1: Generate2[O1]) = new {
          def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT](f: O1 => (D1, D2, D3, D4, D5)): Self2 = self2 :+
            GenerateVto5V(resolve1(f1), ttqkpath5[D1, D2, D3, D4, D5](a, b, c, d, e), wrap15(f)) } }

}

// ===========================================================================
