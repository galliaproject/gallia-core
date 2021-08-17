package gallia.heads.common

import gallia._
import gallia.FunctionWrappers._
import gallia.actions.ActionsUUFission._
import gallia.target.utils.TypedTargetQueryUtils._

// ===========================================================================
trait HeadCommonFission[F <: HeadCommon[F]] { _: HeadCommon[F] =>
  import TSL.FuseFission._

  // ---------------------------------------------------------------------------
  @Max10
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

}

// ===========================================================================
