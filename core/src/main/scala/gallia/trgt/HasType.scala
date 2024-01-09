package gallia
package trgt

import FunctionWrappers._

// ===========================================================================
trait HasType extends HasTypeDuo with HasTypeSeq {
    final override def hts: Seq[HasType] = Seq(this) // from HasTypeSeq

    // ===========================================================================
    // data

    def wrapc(that: HasTypeNode, f: _ff11): _ff11 = x => that.toDynamic(f(this.toStatic(x)))

    // 240108113936 - see corresponding TODO: t240108113850
    def wrapc(that: HasTypes2  , f: _ff12): _ff12 = x => { val (y1, y2)                                 = f(this.toStatic(x)); (that.ht1.toDynamic(y1), that.ht2.toDynamic(y2)) }
    def wrapc(that: HasTypes3  , f: _ff13): _ff13 = x => { val (y1, y2, y3)                             = f(this.toStatic(x)); (that.ht1.toDynamic(y1), that.ht2.toDynamic(y2), that.ht3.toDynamic(y3)) }
    def wrapc(that: HasTypes4  , f: _ff14): _ff14 = x => { val (y1, y2, y3, y4)                         = f(this.toStatic(x)); (that.ht1.toDynamic(y1), that.ht2.toDynamic(y2), that.ht3.toDynamic(y3), that.ht4.toDynamic(y4)) }
    def wrapc(that: HasTypes5  , f: _ff15): _ff15 = x => { val (y1, y2, y3, y4, y5)                     = f(this.toStatic(x)); (that.ht1.toDynamic(y1), that.ht2.toDynamic(y2), that.ht3.toDynamic(y3), that.ht4.toDynamic(y4), that.ht5.toDynamic(y5)) }
    def wrapc(that: HasTypes6  , f: _ff16): _ff16 = x => { val (y1, y2, y3, y4, y5, y6)                 = f(this.toStatic(x)); (that.ht1.toDynamic(y1), that.ht2.toDynamic(y2), that.ht3.toDynamic(y3), that.ht4.toDynamic(y4), that.ht5.toDynamic(y5), that.ht6.toDynamic(y6)) }
    def wrapc(that: HasTypes7  , f: _ff17): _ff17 = x => { val (y1, y2, y3, y4, y5, y6, y7)             = f(this.toStatic(x)); (that.ht1.toDynamic(y1), that.ht2.toDynamic(y2), that.ht3.toDynamic(y3), that.ht4.toDynamic(y4), that.ht5.toDynamic(y5), that.ht6.toDynamic(y6), that.ht7.toDynamic(y7)) }
    def wrapc(that: HasTypes8  , f: _ff18): _ff18 = x => { val (y1, y2, y3, y4, y5, y6, y7, y8)         = f(this.toStatic(x)); (that.ht1.toDynamic(y1), that.ht2.toDynamic(y2), that.ht3.toDynamic(y3), that.ht4.toDynamic(y4), that.ht5.toDynamic(y5), that.ht6.toDynamic(y6), that.ht7.toDynamic(y7), that.ht8.toDynamic(y8)) }
    def wrapc(that: HasTypes9  , f: _ff19): _ff19 = x => { val (y1, y2, y3, y4, y5, y6, y7, y8, y9)     = f(this.toStatic(x)); (that.ht1.toDynamic(y1), that.ht2.toDynamic(y2), that.ht3.toDynamic(y3), that.ht4.toDynamic(y4), that.ht5.toDynamic(y5), that.ht6.toDynamic(y6), that.ht7.toDynamic(y7), that.ht8.toDynamic(y8), that.ht9.toDynamic(y9)) }
    def wrapc(that: HasTypes10 , f: _ff1A): _ff1A = x => { val (y1, y2, y3, y4, y5, y6, y7, y8, y9, yA) = f(this.toStatic(x)); (that.ht1.toDynamic(y1), that.ht2.toDynamic(y2), that.ht3.toDynamic(y3), that.ht4.toDynamic(y4), that.ht5.toDynamic(y5), that.ht6.toDynamic(y6), that.ht7.toDynamic(y7), that.ht8.toDynamic(y8), that.ht9.toDynamic(y9), that.ht10.toDynamic(yA)) }

    // ===========================================================================
    /** dynamic -> static */
    private[trgt] def toStatic(value: Any): Any =
      if (typeNode.isContainedWhatever)
        if (typeNode.isNotOne) ??? //TODO:?
        else               new gallia.Whatever(value)
      else if (!typeNode.isContainedDataClass) value
      // wouldn't need to recompute info if we had result cls here (TODO: t230822103631)
      else                                     typeNode.forceNonBObjInfo.pipe(instantiatorOpt.get.dynamicToStatic(value)) }

  // ===========================================================================
  trait HasInstantiatorOpt { val instantiatorOpt: Option[Instantiator] }

  // ===========================================================================
  trait HasTypes2 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2)
    def wrapc(thatHt: HasTypeNode, f: _ff21): _ff21 = (x1, x2) => thatHt.toDynamic( f(ht1.toStatic(x1), ht2.toStatic(x2)) ) }

  // ===========================================================================
  trait HasTypes3 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3)
    def wrapc(thatHt: HasTypeNode, f: _ff31): _ff31 = (x1, x2, x3) => thatHt.toDynamic( f(ht1.toStatic(x1), ht2.toStatic(x2), ht3.toStatic(x3)) ) }

  // ===========================================================================
  trait HasTypes4 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4)
    def wrapc(thatHt: HasTypeNode, f: _ff41): _ff41 = (x1, x2, x3, x4) => thatHt.toDynamic( f(ht1.toStatic(x1), ht2.toStatic(x2), ht3.toStatic(x3), ht4.toStatic(x4)) ) }

  // ===========================================================================
  trait HasTypes5 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5)
    def wrapc(thatHt: HasTypeNode, f: _ff51): _ff51 = (x1, x2, x3, x4, x5) => thatHt.toDynamic( f(ht1.toStatic(x1), ht2.toStatic(x2), ht3.toStatic(x3), ht4.toStatic(x4), ht5.toStatic(x5)) ) }

  // ===========================================================================
  trait HasTypes6 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6)
    def wrapc(thatHt: HasTypeNode, f: _ff61): _ff61 = (x1, x2, x3, x4, x5, x6) => thatHt.toDynamic( f(ht1.toStatic(x1), ht2.toStatic(x2), ht3.toStatic(x3), ht4.toStatic(x4), ht5.toStatic(x5), ht6.toStatic(x6)) ) }

  // ===========================================================================
  trait HasTypes7 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType; def ht7: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6, ht7)
    def wrapc(thatHt: HasTypeNode, f: _ff71): _ff71 = (x1, x2, x3, x4, x5, x6, x7) => thatHt.toDynamic( f(ht1.toStatic(x1), ht2.toStatic(x2), ht3.toStatic(x3), ht4.toStatic(x4), ht5.toStatic(x5), ht6.toStatic(x6), ht7.toStatic(x7)) ) }

  // ===========================================================================
  trait HasTypes8 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType; def ht7: HasType; def ht8: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6, ht7, ht8)
    def wrapc(thatHt: HasTypeNode, f: _ff81): _ff81 = (x1, x2, x3, x4, x5, x6, x7, x8) => thatHt.toDynamic( f(ht1.toStatic(x1), ht2.toStatic(x2), ht3.toStatic(x3), ht4.toStatic(x4), ht5.toStatic(x5), ht6.toStatic(x6), ht7.toStatic(x7), ht8.toStatic(x8)) ) }

  // ===========================================================================
  trait HasTypes9 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType; def ht7: HasType; def ht8: HasType; def ht9: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6, ht7, ht8, ht9)
    def wrapc(thatHt: HasTypeNode, f: _ff91): _ff91 = (x1, x2, x3, x4, x5, x6, x7, x8, x9) => thatHt.toDynamic( f(ht1.toStatic(x1), ht2.toStatic(x2), ht3.toStatic(x3), ht4.toStatic(x4), ht5.toStatic(x5), ht6.toStatic(x6), ht7.toStatic(x7), ht8.toStatic(x8), ht9.toStatic(x9)) ) }

  // ===========================================================================
  trait HasTypes10 extends HasTypeSeq {
    def ht1: HasType; def ht2: HasType; def ht3: HasType; def ht4: HasType; def ht5: HasType; def ht6: HasType; def ht7: HasType; def ht8: HasType; def ht9: HasType; def ht10: HasType
    def hts: Seq[HasType] = Seq(ht1, ht2, ht3, ht4, ht5, ht6, ht7, ht8, ht9, ht10)
    def wrapc(thatHt: HasTypeNode, f: _ffA1): _ffA1 = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) => thatHt.toDynamic( f(ht1.toStatic(x1), ht2.toStatic(x2), ht3.toStatic(x3), ht4.toStatic(x4), ht5.toStatic(x5), ht6.toStatic(x6), ht7.toStatic(x7), ht8.toStatic(x8), ht9.toStatic(x9), ht10.toStatic(x10)) ) }

// ===========================================================================
