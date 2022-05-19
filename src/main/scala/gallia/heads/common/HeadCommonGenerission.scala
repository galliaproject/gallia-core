package gallia
package heads.common

import FunctionWrappers._
import actions.common.ActionsCommonGenerission._

// ===========================================================================
trait HeadCommonGenerission[F <: HeadCommon[F]] { ignored: HeadCommon[F] =>
  import TSL.Generate2.resolve  

  // ===========================================================================
  //TODO: t210111171545 - validate origin isn't u or z
  def generate(d1: KPathW, d2: KPathW) = new {
      def from[O1: WTT](o1: Generate2[O1]) = new {
        def using[D1: WTT, D2: WTT](f: O1 => (D1, D2)): Self2 = self2 :+
          GenerateVto2V(resolve(o1), tkpaths2[D1, D2](d1, d2), wrap12(f)) } }

    // ---------------------------------------------------------------------------
    def generate(d1: KPathW, d2: KPathW, d3: KPathW) = new {
      def from[O1: WTT](o1: Generate2[O1]) = new {
        def using[D1: WTT, D2: WTT, D3: WTT](f: O1 => (D1, D2, D3)): Self2 = self2 :+
          GenerateVto3V(resolve(o1), tkpaths3[D1, D2, D3](d1, d2, d3), wrap13(f)) } }

    // ---------------------------------------------------------------------------
    def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW) = new {
      def from[O1: WTT](o1: Generate2[O1]) = new {
        def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT](f: O1 => (D1, D2, D3, D4)): Self2 = self2 :+
          GenerateVto4V(resolve(o1), tkpaths4[D1, D2, D3, D4](d1, d2, d3, d4), wrap14(f)) } }

    // ---------------------------------------------------------------------------
    def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW) = new {
      def from[O1: WTT](o1: Generate2[O1]) = new {
        def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT](f: O1 => (D1, D2, D3, D4, D5)): Self2 = self2 :+
          GenerateVto5V(resolve(o1), tkpaths5[D1, D2, D3, D4, D5](d1, d2, d3, d4, d5), wrap15(f)) } }

    // ---------------------------------------------------------------------------
    def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW) = new {
      def from[O1: WTT](o1: Generate2[O1]) = new {
        def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT](f: O1 => (D1, D2, D3, D4, D5, D6)): Self2 = self2 :+
          GenerateVto6V(resolve(o1), tkpaths6[D1, D2, D3, D4, D5, D6](d1, d2, d3, d4, d5, d6), wrap16(f)) } }

    // ---------------------------------------------------------------------------
    def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW) = new {
      def from[O1: WTT](o1: Generate2[O1]) = new {
        def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7)): Self2 = self2 :+
          GenerateVto7V(resolve(o1), tkpaths7[D1, D2, D3, D4, D5, D6, D7](d1, d2, d3, d4, d5, d6, d7), wrap17(f)) } }

    // ---------------------------------------------------------------------------
    def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW) = new {
      def from[O1: WTT](o1: Generate2[O1]) = new {
        def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8)): Self2 = self2 :+
          GenerateVto8V(resolve(o1), tkpaths8[D1, D2, D3, D4, D5, D6, D7, D8](d1, d2, d3, d4, d5, d6, d7, d8), wrap18(f)) } }

    // ---------------------------------------------------------------------------
    def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW, d9: KPathW) = new {
      def from[O1: WTT](o1: Generate2[O1]) = new {
        def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT, D9: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8, D9)): Self2 = self2 :+
          GenerateVto9V(resolve(o1), tkpaths9[D1, D2, D3, D4, D5, D6, D7, D8, D9](d1, d2, d3, d4, d5, d6, d7, d8, d9), wrap19(f)) } }
    
    // ---------------------------------------------------------------------------
    def generate(d1: KPathW, d2: KPathW, d3: KPathW, d4: KPathW, d5: KPathW, d6: KPathW, d7: KPathW, d8: KPathW, d9: KPathW, d10: KPathW) = new {
      def from[O1: WTT](o1: Generate2[O1]) = new {
        def using[D1: WTT, D2: WTT, D3: WTT, D4: WTT, D5: WTT, D6: WTT, D7: WTT, D8: WTT, D9: WTT, D10: WTT](f: O1 => (D1, D2, D3, D4, D5, D6, D7, D8, D9, D10)): Self2 = self2 :+
          GenerateVto10V(resolve(o1), tkpaths10[D1, D2, D3, D4, D5, D6, D7, D8, D9, D10](d1, d2, d3, d4, d5, d6, d7, d8, d9, d10), wrap1A(f)) } }
}

// ===========================================================================
