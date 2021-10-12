package gallia.target

import gallia.FunctionWrappers._

// ===========================================================================
trait HasType extends HasTypeNode with HasTypeSeq {
    val instantiator: Instantiator

    // ---------------------------------------------------------------------------
    def hts: Seq[HasType] = Seq(this)

    // ===========================================================================
    // data

    def wrapc(that: HasTypeNode, f: _ff11): _ff11 = x => that._out(f(this._in(x)))

    def wrapc(that: HasTypes2  , f: _ff12): _ff12 = x => { val (y1, y2)                                 = f(this._in(x)); (that.ht1._out(y1), that.ht2._out(y2)) }
    def wrapc(that: HasTypes3  , f: _ff13): _ff13 = x => { val (y1, y2, y3)                             = f(this._in(x)); (that.ht1._out(y1), that.ht2._out(y2), that.ht3._out(y3)) }
    def wrapc(that: HasTypes4  , f: _ff14): _ff14 = x => { val (y1, y2, y3, y4)                         = f(this._in(x)); (that.ht1._out(y1), that.ht2._out(y2), that.ht3._out(y3), that.ht4._out(y4)) }
    def wrapc(that: HasTypes5  , f: _ff15): _ff15 = x => { val (y1, y2, y3, y4, y5)                     = f(this._in(x)); (that.ht1._out(y1), that.ht2._out(y2), that.ht3._out(y3), that.ht4._out(y4), that.ht5._out(y5)) }
    def wrapc(that: HasTypes6  , f: _ff16): _ff16 = x => { val (y1, y2, y3, y4, y5, y6)                 = f(this._in(x)); (that.ht1._out(y1), that.ht2._out(y2), that.ht3._out(y3), that.ht4._out(y4), that.ht5._out(y5), that.ht6._out(y6)) }
    def wrapc(that: HasTypes7  , f: _ff17): _ff17 = x => { val (y1, y2, y3, y4, y5, y6, y7)             = f(this._in(x)); (that.ht1._out(y1), that.ht2._out(y2), that.ht3._out(y3), that.ht4._out(y4), that.ht5._out(y5), that.ht6._out(y6), that.ht7._out(y7)) }
    def wrapc(that: HasTypes8  , f: _ff18): _ff18 = x => { val (y1, y2, y3, y4, y5, y6, y7, y8)         = f(this._in(x)); (that.ht1._out(y1), that.ht2._out(y2), that.ht3._out(y3), that.ht4._out(y4), that.ht5._out(y5), that.ht6._out(y6), that.ht7._out(y7), that.ht8._out(y8)) }
    def wrapc(that: HasTypes9  , f: _ff19): _ff19 = x => { val (y1, y2, y3, y4, y5, y6, y7, y8, y9)     = f(this._in(x)); (that.ht1._out(y1), that.ht2._out(y2), that.ht3._out(y3), that.ht4._out(y4), that.ht5._out(y5), that.ht6._out(y6), that.ht7._out(y7), that.ht8._out(y8), that.ht9._out(y9)) }
    def wrapc(that: HasTypes10 , f: _ff1A): _ff1A = x => { val (y1, y2, y3, y4, y5, y6, y7, y8, y9, yA) = f(this._in(x)); (that.ht1._out(y1), that.ht2._out(y2), that.ht3._out(y3), that.ht4._out(y4), that.ht5._out(y5), that.ht6._out(y6), that.ht7._out(y7), that.ht8._out(y8), that.ht9._out(y9), that.ht10._out(yA)) }

    // ===========================================================================
    private[target] def _in(value: Any): Any =
      if (node.isContainedWhatever)
        if (node.isNotOne) ??? //TODO:?
        else               new gallia.Whatever(value)
      else                 DataClassUtils._in(node, instantiator)(value)
  }

  // ===========================================================================
  trait HasTypes2 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2)
    def wrapc(thatHt: HasTypeNode, f: _ff21): _ff21 = (x1, x2) => thatHt._out( f(ht1._in(x1), ht2._in(x2)) ) }

  // ===========================================================================
  trait HasTypes3 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3)
    def wrapc(thatHt: HasTypeNode, f: _ff31): _ff31 = (x1, x2, x3) => thatHt._out( f(ht1._in(x1), ht2._in(x2), ht3._in(x3)) ) }

  // ===========================================================================
  trait HasTypes4 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4)
    def wrapc(thatHt: HasTypeNode, f: _ff41): _ff41 = (x1, x2, x3, x4) => thatHt._out( f(ht1._in(x1), ht2._in(x2), ht3._in(x3), ht4._in(x4)) ) }

  // ===========================================================================
  trait HasTypes5 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5)
    def wrapc(thatHt: HasTypeNode, f: _ff51): _ff51 = (x1, x2, x3, x4, x5) => thatHt._out( f(ht1._in(x1), ht2._in(x2), ht3._in(x3), ht4._in(x4), ht5._in(x5)) ) }

  // ===========================================================================
  trait HasTypes6 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6)
    def wrapc(thatHt: HasTypeNode, f: _ff61): _ff61 = (x1, x2, x3, x4, x5, x6) => thatHt._out( f(ht1._in(x1), ht2._in(x2), ht3._in(x3), ht4._in(x4), ht5._in(x5), ht6._in(x6)) ) }

  // ===========================================================================
  trait HasTypes7 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType; def ht7: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6, ht7)
    def wrapc(thatHt: HasTypeNode, f: _ff71): _ff71 = (x1, x2, x3, x4, x5, x6, x7) => thatHt._out( f(ht1._in(x1), ht2._in(x2), ht3._in(x3), ht4._in(x4), ht5._in(x5), ht6._in(x6), ht7._in(x7)) ) }

  // ===========================================================================
  trait HasTypes8 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType; def ht7: HasType; def ht8: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6, ht7, ht8)
    def wrapc(thatHt: HasTypeNode, f: _ff81): _ff81 = (x1, x2, x3, x4, x5, x6, x7, x8) => thatHt._out( f(ht1._in(x1), ht2._in(x2), ht3._in(x3), ht4._in(x4), ht5._in(x5), ht6._in(x6), ht7._in(x7), ht8._in(x8)) ) }

  // ===========================================================================
  trait HasTypes9 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType; def ht7: HasType; def ht8: HasType; def ht9: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6, ht7, ht8, ht9)
    def wrapc(thatHt: HasTypeNode, f: _ff91): _ff91 = (x1, x2, x3, x4, x5, x6, x7, x8, x9) => thatHt._out( f(ht1._in(x1), ht2._in(x2), ht3._in(x3), ht4._in(x4), ht5._in(x5), ht6._in(x6), ht7._in(x7), ht8._in(x8), ht9._in(x9)) ) }
  
  // ===========================================================================
  trait HasTypes10 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType; def ht7: HasType; def ht8: HasType; def ht9: HasType; def ht10: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6, ht7, ht8, ht9, ht10)
    def wrapc(thatHt: HasTypeNode, f: _ffA1): _ffA1 = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) => thatHt._out( f(ht1._in(x1), ht2._in(x2), ht3._in(x3), ht4._in(x4), ht5._in(x5), ht6._in(x6), ht7._in(x7), ht8._in(x8), ht9._in(x9), ht10._in(x10)) ) }
    
// ===========================================================================
