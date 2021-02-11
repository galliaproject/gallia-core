package gallia.target

import gallia.FunctionWrappers._

// ===========================================================================
trait HasType extends HasTypeNode with HasTypeSeq {
    val instantiator: Instantiator

    // ---------------------------------------------------------------------------
    def hts: Seq[HasType] = Seq(this)

    // ===========================================================================
    // data

    def wrapc(that: HasTypeNode, f: _ff11): _ff11 = x => that.out(f(this.in(x)))

    def wrapc(that: HasTypes2  , f: _ff12): _ff12 = x => { val (y1, y2)                         = f(this.in(x)); (that.ht1.out(y1), that.ht2.out(y2)) }
    def wrapc(that: HasTypes3  , f: _ff13): _ff13 = x => { val (y1, y2, y3)                     = f(this.in(x)); (that.ht1.out(y1), that.ht2.out(y2), that.ht3.out(y3)) }
    def wrapc(that: HasTypes4  , f: _ff14): _ff14 = x => { val (y1, y2, y3, y4)                 = f(this.in(x)); (that.ht1.out(y1), that.ht2.out(y2), that.ht3.out(y3), that.ht4.out(y4)) }
    def wrapc(that: HasTypes5  , f: _ff15): _ff15 = x => { val (y1, y2, y3, y4, y5)             = f(this.in(x)); (that.ht1.out(y1), that.ht2.out(y2), that.ht3.out(y3), that.ht4.out(y4), that.ht5.out(y5)) }
    def wrapc(that: HasTypes6  , f: _ff16): _ff16 = x => { val (y1, y2, y3, y4, y5, y6)         = f(this.in(x)); (that.ht1.out(y1), that.ht2.out(y2), that.ht3.out(y3), that.ht4.out(y4), that.ht5.out(y5), that.ht6.out(y6)) }
    def wrapc(that: HasTypes7  , f: _ff17): _ff17 = x => { val (y1, y2, y3, y4, y5, y6, y7)     = f(this.in(x)); (that.ht1.out(y1), that.ht2.out(y2), that.ht3.out(y3), that.ht4.out(y4), that.ht5.out(y5), that.ht6.out(y6), that.ht7.out(y7)) }
    def wrapc(that: HasTypes8  , f: _ff18): _ff18 = x => { val (y1, y2, y3, y4, y5, y6, y7, y8) = f(this.in(x)); (that.ht1.out(y1), that.ht2.out(y2), that.ht3.out(y3), that.ht4.out(y4), that.ht5.out(y5), that.ht6.out(y6), that.ht7.out(y7), that.ht8.out(y8)) }

    // ===========================================================================
    def in(value: Any): Any =
      if (node.isContainedWhatever)
        if (node.isNotOne) ??? //TODO:?
        else               new gallia.Whatever(value)
      else                 DataClassUtils.in(node, instantiator)(value)
  }

  // ===========================================================================
  trait HasTypes2 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2)
    def wrapc(thatHt: HasTypeNode, f: _ff21): _ff21 = (x1, x2) => thatHt.out( f(ht1.in(x1), ht2.in(x2)) ) }

  // ===========================================================================
  trait HasTypes3 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3)
    def wrapc(thatHt: HasTypeNode, f: _ff31): _ff31 = (x1, x2, x3) => thatHt.out( f(ht1.in(x1), ht2.in(x2), ht3.in(x3)) ) }

  // ===========================================================================
  trait HasTypes4 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4)
    def wrapc(thatHt: HasTypeNode, f: _ff41): _ff41 = (x1, x2, x3, x4) => thatHt.out( f(ht1.in(x1), ht2.in(x2), ht3.in(x3), ht4.in(x4)) ) }

  // ===========================================================================
  trait HasTypes5 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5)
    def wrapc(thatHt: HasTypeNode, f: _ff51): _ff51 = (x1, x2, x3, x4, x5) => thatHt.out( f(ht1.in(x1), ht2.in(x2), ht3.in(x3), ht4.in(x4), ht5.in(x5)) ) }

  // ===========================================================================
  trait HasTypes6 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6)
    def wrapc(thatHt: HasTypeNode, f: _ff61): _ff61 = (x1, x2, x3, x4, x5, x6) => thatHt.out( f(ht1.in(x1), ht2.in(x2), ht3.in(x3), ht4.in(x4), ht5.in(x5), ht6.in(x6)) ) }

  // ===========================================================================
  trait HasTypes7 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType; def ht7: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6, ht7)
    def wrapc(thatHt: HasTypeNode, f: _ff71): _ff71 = (x1, x2, x3, x4, x5, x6, x7) => thatHt.out( f(ht1.in(x1), ht2.in(x2), ht3.in(x3), ht4.in(x4), ht5.in(x5), ht6.in(x6), ht7.in(x7)) ) }

  // ===========================================================================
  trait HasTypes8 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType; def ht7: HasType; def ht8: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6, ht7, ht8)
    def wrapc(thatHt: HasTypeNode, f: _ff81): _ff81 = (x1, x2, x3, x4, x5, x6, x7, x8) => thatHt.out( f(ht1.in(x1), ht2.in(x2), ht3.in(x3), ht4.in(x4), ht5.in(x5), ht6.in(x6), ht7.in(x7), ht8.in(x8)) ) }

// ===========================================================================