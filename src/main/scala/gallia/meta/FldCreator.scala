package gallia
package meta

import aptus.Nes

import reflect.BasicType._

// ===========================================================================
@TypeMatching
trait FldCreator { import Fld._
  protected val _key: Key

  // ---------------------------------------------------------------------------
  def typed[T : WTT]: Fld = Fld(_key, Info.forceFrom[T])

  def union(first: Info, more: Info*): Fld = Fld(_key, (first +: more)) // see t210125111338 (union types)

  // ===========================================================================
  def string    : Fld = one(_key, _String)
  def string_   : Fld = opt(_key, _String)
  def strings   : Fld = nes(_key, _String)
  def strings_  : Fld = pes(_key, _String)

  def int       : Fld = one(_key, _Int)
  def int_      : Fld = opt(_key, _Int)
  def ints      : Fld = nes(_key, _Int)
  def ints_     : Fld = pes(_key, _Int)

  def double    : Fld = one(_key, _Double)
  def double_   : Fld = opt(_key, _Double)
  def doubles   : Fld = nes(_key, _Double)
  def doubles_  : Fld = pes(_key, _Double)

  def boolean   : Fld = one(_key, _Boolean)
  def boolean_  : Fld = opt(_key, _Boolean)
  def booleans  : Fld = nes(_key, _Boolean)
  def booleans_ : Fld = pes(_key, _Boolean)

  // ---------------------------------------------------------------------------
  def byte    : Fld = one(_key, _Byte)
  def byte_   : Fld = opt(_key, _Byte)
  def bytes   : Fld = nes(_key, _Byte) // not to be confused with binary...
  def bytes_  : Fld = pes(_key, _Byte) // not to be confused with binary...

  def short   : Fld = one(_key, _Short)
  def short_  : Fld = opt(_key, _Short)
  def shorts  : Fld = nes(_key, _Short)
  def shorts_ : Fld = pes(_key, _Short)

  def long    : Fld = one(_key, _Long)
  def long_   : Fld = opt(_key, _Long)
  def longs   : Fld = nes(_key, _Long)
  def longs_  : Fld = pes(_key, _Long)

  def float   : Fld = one(_key, _Float)
  def float_  : Fld = opt(_key, _Float)
  def floats  : Fld = nes(_key, _Float)
  def floats_ : Fld = pes(_key, _Float)

  // ---------------------------------------------------------------------------
  def bigInt   : Fld = one(_key, _BigInt)
  def bigInt_  : Fld = opt(_key, _BigInt)
  def bigInts  : Fld = nes(_key, _BigInt)
  def bigInts_ : Fld = pes(_key, _BigInt)

  def bigDec   : Fld = one(_key, _BigDec)
  def bigDec_  : Fld = opt(_key, _BigDec)
  def bigDecs  : Fld = nes(_key, _BigDec)
  def bigDecs_ : Fld = pes(_key, _BigDec)

  // ---------------------------------------------------------------------------
  def localDate       : Fld = one(_key, _LocalDate)  
  def localDate_      : Fld = opt(_key, _LocalDate)
  def localDates      : Fld = nes(_key, _LocalDate)
  def localDates_     : Fld = pes(_key, _LocalDate)

  def localTime       : Fld = one(_key, _LocalTime)  
  def localTime_      : Fld = opt(_key, _LocalTime)
  def localTimes      : Fld = nes(_key, _LocalTime)
  def localTimes_     : Fld = pes(_key, _LocalTime)
  
  def localDateTime   : Fld = one(_key, _LocalDateTime)
  def localDateTime_  : Fld = opt(_key, _LocalDateTime)
  def localDateTimes  : Fld = nes(_key, _LocalDateTime)
  def localDateTimes_ : Fld = pes(_key, _LocalDateTime)

  def offsetDateTime   : Fld = one(_key, _OffsetDateTime)
  def offsetDateTime_  : Fld = opt(_key, _OffsetDateTime)
  def offsetDateTimes  : Fld = nes(_key, _OffsetDateTime)
  def offsetDateTimes_ : Fld = pes(_key, _OffsetDateTime)

  def zonedDateTime   : Fld = one(_key, _ZonedDateTime)
  def zonedDateTime_  : Fld = opt(_key, _ZonedDateTime)
  def zonedDateTimes  : Fld = nes(_key, _ZonedDateTime)
  def zonedDateTimes_ : Fld = pes(_key, _ZonedDateTime)

  def instant   : Fld = one(_key, _Instant)
  def instant_  : Fld = opt(_key, _Instant)
  def instants  : Fld = nes(_key, _Instant)
  def instants_ : Fld = pes(_key, _Instant)
  
  // ---------------------------------------------------------------------------
  //TODO: enum - t210201095414

  // ---------------------------------------------------------------------------
  def binary    : Fld = one(_key, _Binary)
  def binary_   : Fld = opt(_key, _Binary)
  def binarys   : Fld = nes(_key, _Binary)
  def binarys_  : Fld = pes(_key, _Binary)
  
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
