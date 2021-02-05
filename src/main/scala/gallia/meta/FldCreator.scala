package gallia.meta

import aptus.Nes

import gallia._
import gallia.reflect.Container._
import gallia.reflect.BasicType._

// ===========================================================================
@gallia.TypeMatching
trait FldCreator {
  protected val _key: Key

  // ---------------------------------------------------------------------------
  def typed[T : WTT]: Fld = Fld(_key, Info.forceFrom[T])

  // ---------------------------------------------------------------------------
  def string    : Fld = Fld(_key, Info.from(_One, _String))
  def string_   : Fld = Fld(_key, Info.from(_Opt, _String))
  def strings   : Fld = Fld(_key, Info.from(_Nes, _String))
  def strings_  : Fld = Fld(_key, Info.from(_Pes, _String))

  def int       : Fld = Fld(_key, Info.from(_One, _Int))
  def int_      : Fld = Fld(_key, Info.from(_Opt, _Int))
  def ints      : Fld = Fld(_key, Info.from(_Nes, _Int))
  def ints_     : Fld = Fld(_key, Info.from(_Pes, _Int))

  def double    : Fld = Fld(_key, Info.from(_One, _Double))
  def double_   : Fld = Fld(_key, Info.from(_Opt, _Double))
  def doubles   : Fld = Fld(_key, Info.from(_Nes, _Double))
  def doubles_  : Fld = Fld(_key, Info.from(_Pes, _Double))

  def boolean   : Fld = Fld(_key, Info.from(_One, _Boolean))
  def boolean_  : Fld = Fld(_key, Info.from(_Opt, _Boolean))
  def booleans  : Fld = Fld(_key, Info.from(_Nes, _Boolean))
  def booleans_ : Fld = Fld(_key, Info.from(_Pes, _Boolean))

  // ---------------------------------------------------------------------------
  def byte    : Fld = Fld(_key, Info.from(_One, _Byte))
  def byte_   : Fld = Fld(_key, Info.from(_Opt, _Byte))
  def bytes   : Fld = Fld(_key, Info.from(_Nes, _Byte))
  def bytes_  : Fld = Fld(_key, Info.from(_Pes, _Byte))

  def short   : Fld = Fld(_key, Info.from(_One, _Short))
  def short_  : Fld = Fld(_key, Info.from(_Opt, _Short))
  def shorts  : Fld = Fld(_key, Info.from(_Nes, _Short))
  def shorts_ : Fld = Fld(_key, Info.from(_Pes, _Short))

  def long    : Fld = Fld(_key, Info.from(_One, _Long))
  def long_   : Fld = Fld(_key, Info.from(_Opt, _Long))
  def longs   : Fld = Fld(_key, Info.from(_Nes, _Long))
  def longs_  : Fld = Fld(_key, Info.from(_Pes, _Long))

  def float   : Fld = Fld(_key, Info.from(_One, _Float))
  def float_  : Fld = Fld(_key, Info.from(_Opt, _Float))
  def floats  : Fld = Fld(_key, Info.from(_Nes, _Float))
  def floats_ : Fld = Fld(_key, Info.from(_Pes, _Float))

  // ---------------------------------------------------------------------------
  def bigInt       : Fld = Fld(_key, Info.from(_One, _BigInt))
  def bigInt_      : Fld = Fld(_key, Info.from(_Opt, _BigInt))
  def bigInts      : Fld = Fld(_key, Info.from(_Nes, _BigInt))
  def bigInts_     : Fld = Fld(_key, Info.from(_Pes, _BigInt))

  def bigDecimal   : Fld = Fld(_key, Info.from(_One, _BigDecimal))
  def bigDecimal_  : Fld = Fld(_key, Info.from(_Opt, _BigDecimal))
  def bigDecimals  : Fld = Fld(_key, Info.from(_Nes, _BigDecimal))
  def bigDecimals_ : Fld = Fld(_key, Info.from(_Pes, _BigDecimal))

//def date       : Fld = Fld(_key, Info.from(_One, _LocalDate)) -- t210202100936 - conflict with aptus' ... as date1 ?
  def date_      : Fld = Fld(_key, Info.from(_Opt, _LocalDate))
  def dates      : Fld = Fld(_key, Info.from(_Nes, _LocalDate))
  def dates_     : Fld = Fld(_key, Info.from(_Pes, _LocalDate))

//def dateTime   : Fld = Fld(_key, Info.from(_One, _LocalDateTime)) -- conflict with aptus' ... as dateTime1 ?
  def dateTime_  : Fld = Fld(_key, Info.from(_Opt, _LocalDateTime))
  def dateTimes  : Fld = Fld(_key, Info.from(_Nes, _LocalDateTime))
  def dateTimes_ : Fld = Fld(_key, Info.from(_Pes, _LocalDateTime))

  // ---------------------------------------------------------------------------
  //TODO: enum - t210201095414

  // ===========================================================================
  def cls_ (fields: Nes[Fld]): Fld = Fld(_key, Info.opt(Cls(fields)))
  def cls  (fields: Nes[Fld]): Fld = Fld(_key, Info.one(Cls(fields)))
  def clss (fields: Nes[Fld]): Fld = Fld(_key, Info.nes(Cls(fields)))
  def clss_(fields: Nes[Fld]): Fld = Fld(_key, Info.pes(Cls(fields)))

  def cls_ (field1: Fld, more: Fld*): Fld = Fld(_key, Info.opt(Cls(field1 +: more)))
  def cls  (field1: Fld, more: Fld*): Fld = Fld(_key, Info.one(Cls(field1 +: more)))
  def clss (field1: Fld, more: Fld*): Fld = Fld(_key, Info.nes(Cls(field1 +: more)))
  def clss_(field1: Fld, more: Fld*): Fld = Fld(_key, Info.pes(Cls(field1 +: more)))

  def cls_ (c: Cls): Fld = Fld(_key, Info.opt(Cls(c.fields)))
  def cls  (c: Cls): Fld = Fld(_key, Info.one(Cls(c.fields)))
  def clss (c: Cls): Fld = Fld(_key, Info.nes(Cls(c.fields)))
  def clss_(c: Cls): Fld = Fld(_key, Info.pes(Cls(c.fields)))

}

// ===========================================================================
