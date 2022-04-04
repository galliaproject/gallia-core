package gallia
package selection.typed.fluency

// ===========================================================================
@TypeMatching // very boilerplatey; TODO: t210124092716 - codegen (very boilerplaty)
object TsSelIndividual {

  // ===========================================================================
  import selection.untyped.UtsOps._

  // ---------------------------------------------------------------------------
  private object _AllSelections //TODO: list them all (codegen:x)
        extends
                ForKey
          with  ForPath
          with  ForEachKey
          with  ForEachPath

          with  CommonTyped
          with  Rename
          with  Remove
          with  Retain

          with  SetDefault
          with  Translate
          with  RemoveIf

          with  Transform

    // ---------------------------------------------------------------------------
    private[typed] def allSelections[A] = _AllSelections.asInstanceOf[A]

  // ===========================================================================  
  trait HasSelNonXTyped[A, Ignored] { def typed [T: WTT](sel: A => Any) = new One_[T](sel(allSelections[A])) }
  trait HasSelXTyped   [A, Ignored] { def typedx[T: WTT](sel: A => Any) = new One_[T](sel(allSelections[A]), ignoreContainer = true) }
  
  // ===========================================================================
  trait HasSelOneString [A, Ignored] { def string (sel: A => Any) = new One_[String ](sel(allSelections[A])) }
  trait HasSelOneInt    [A, Ignored] { def int    (sel: A => Any) = new One_[Int    ](sel(allSelections[A])) }
  trait HasSelOneDouble [A, Ignored] { def double (sel: A => Any) = new One_[Double ](sel(allSelections[A])) }
  trait HasSelOneBoolean[A, Ignored] { def boolean(sel: A => Any) = new One_[Boolean](sel(allSelections[A])) }

  trait HasSelOneByte [A, Ignored] { def byte (sel: A => Any) = new One_[Byte ](sel(allSelections[A])) }
  trait HasSelOneShort[A, Ignored] { def short(sel: A => Any) = new One_[Short](sel(allSelections[A])) }
  trait HasSelOneLong [A, Ignored] { def long (sel: A => Any) = new One_[Long] (sel(allSelections[A])) }
  trait HasSelOneFloat[A, Ignored] { def float(sel: A => Any) = new One_[Float](sel(allSelections[A])) }
  
  trait HasSelOneBigInt[A, Ignored] { def bigInt(sel: A => Any) = new One_[BigInt](sel(allSelections[A])) }
  trait HasSelOneBigDec[A, Ignored] { def bigDec(sel: A => Any) = new One_[BigDec](sel(allSelections[A])) }
  
  trait HasSelOneLocalDate     [A, Ignored] { def localDate     (sel: A => Any) = new One_[LocalDate]     (sel(allSelections[A])) }
  trait HasSelOneLocalTime     [A, Ignored] { def localTime     (sel: A => Any) = new One_[LocalTime]     (sel(allSelections[A])) }
  trait HasSelOneLocalDateTime [A, Ignored] { def localDateTime (sel: A => Any) = new One_[LocalDateTime] (sel(allSelections[A])) }
  trait HasSelOneOffsetDateTime[A, Ignored] { def offsetDateTime(sel: A => Any) = new One_[OffsetDateTime](sel(allSelections[A])) }
  trait HasSelOneZonedDateTime [A, Ignored] { def zonedDateTime (sel: A => Any) = new One_[ZonedDateTime] (sel(allSelections[A])) }
  trait HasSelOneInstant       [A, Ignored] { def instant       (sel: A => Any) = new One_[Instant]       (sel(allSelections[A])) }
  
  trait HasSelOneBinary        [A, Ignored] { def binary        (sel: A => Any) = new One_[ByteBuffer]    (sel(allSelections[A])) }

  // ===========================================================================
  trait HasSelOptString [A, Ignored] { def string_ (sel: A => Any) = new Opt_[String ](sel(allSelections[A])) }
  trait HasSelOptInt    [A, Ignored] { def int_    (sel: A => Any) = new Opt_[Int    ](sel(allSelections[A])) }
  trait HasSelOptDouble [A, Ignored] { def double_ (sel: A => Any) = new Opt_[Double ](sel(allSelections[A])) }
  trait HasSelOptBoolean[A, Ignored] { def boolean_(sel: A => Any) = new Opt_[Boolean](sel(allSelections[A])) }

  trait HasSelOptByte [A, Ignored] { def byte_ (sel: A => Any) = new Opt_[Byte ](sel(allSelections[A])) }
  trait HasSelOptShort[A, Ignored] { def short_(sel: A => Any) = new Opt_[Short](sel(allSelections[A])) }
  trait HasSelOptLong [A, Ignored] { def long_ (sel: A => Any) = new Opt_[Long] (sel(allSelections[A])) }
  trait HasSelOptFloat[A, Ignored] { def float_(sel: A => Any) = new Opt_[Float](sel(allSelections[A])) }
  
  trait HasSelOptBigInt[A, Ignored] { def bigInt_(sel: A => Any) = new Opt_[BigInt](sel(allSelections[A])) }
  trait HasSelOptBigDec[A, Ignored] { def bigDec_(sel: A => Any) = new Opt_[BigDec](sel(allSelections[A])) }

  trait HasSelOptLocalDate     [A, Ignored] { def localDate_     (sel: A => Any) = new Opt_[LocalDate]     (sel(allSelections[A])) }
  trait HasSelOptLocalTime     [A, Ignored] { def localTime_     (sel: A => Any) = new Opt_[LocalTime]     (sel(allSelections[A])) }
  trait HasSelOptLocalDateTime [A, Ignored] { def localDateTime_ (sel: A => Any) = new Opt_[LocalDateTime] (sel(allSelections[A])) }
  trait HasSelOptOffsetDateTime[A, Ignored] { def offsetDateTime_(sel: A => Any) = new Opt_[OffsetDateTime](sel(allSelections[A])) }
  trait HasSelOptZonedDateTime [A, Ignored] { def zonedDateTime_ (sel: A => Any) = new Opt_[ZonedDateTime] (sel(allSelections[A])) }
  trait HasSelOptInstant       [A, Ignored] { def instant_       (sel: A => Any) = new Opt_[Instant]       (sel(allSelections[A])) }
  
  trait HasSelOptBinary        [A, Ignored] { def binary_        (sel: A => Any) = new Opt_[ByteBuffer]    (sel(allSelections[A])) }
  
  // ===========================================================================
  trait HasSelNesString [A, Ignored] { def strings (sel: A => Any) = new Nes_[String ](sel(allSelections[A])) }
  trait HasSelNesInt    [A, Ignored] { def ints    (sel: A => Any) = new Nes_[Int    ](sel(allSelections[A])) }
  trait HasSelNesDouble [A, Ignored] { def doubles (sel: A => Any) = new Nes_[Double ](sel(allSelections[A])) }
  trait HasSelNesBoolean[A, Ignored] { def booleans(sel: A => Any) = new Nes_[Boolean](sel(allSelections[A])) }

  trait HasSelNesByte [A, Ignored] { def bytes (sel: A => Any) = new Nes_[Byte ](sel(allSelections[A])) }
  trait HasSelNesShort[A, Ignored] { def shorts(sel: A => Any) = new Nes_[Short](sel(allSelections[A])) }
  trait HasSelNesLong [A, Ignored] { def longs (sel: A => Any) = new Nes_[Long] (sel(allSelections[A])) }
  trait HasSelNesFloat[A, Ignored] { def floats(sel: A => Any) = new Nes_[Float](sel(allSelections[A])) }
  
  trait HasSelNesBigInt[A, Ignored] { def bigInts(sel: A => Any) = new Nes_[BigInt](sel(allSelections[A])) }
  trait HasSelNesBigDec[A, Ignored] { def bigDecs(sel: A => Any) = new Nes_[BigDec](sel(allSelections[A])) }

  trait HasSelNesLocalDate     [A, Ignored] { def localDates     (sel: A => Any) = new Nes_[LocalDate]     (sel(allSelections[A])) }
  trait HasSelNesLocalTime     [A, Ignored] { def localTimes     (sel: A => Any) = new Nes_[LocalTime]     (sel(allSelections[A])) }
  trait HasSelNesLocalDateTime [A, Ignored] { def localDateTimes (sel: A => Any) = new Nes_[LocalDateTime] (sel(allSelections[A])) }
  trait HasSelNesOffsetDateTime[A, Ignored] { def offsetDateTimes(sel: A => Any) = new Nes_[OffsetDateTime](sel(allSelections[A])) }
  trait HasSelNesZonedDateTime [A, Ignored] { def zonedDateTimes (sel: A => Any) = new Nes_[ZonedDateTime] (sel(allSelections[A])) }
  trait HasSelNesInstant       [A, Ignored] { def instants       (sel: A => Any) = new Nes_[Instant]       (sel(allSelections[A])) }
  
  trait HasSelNesBinary        [A, Ignored] { def binarys        (sel: A => Any) = new Nes_[ByteBuffer]    (sel(allSelections[A])) }
  
  // ===========================================================================
  trait HasSelPesString [A, Ignored] { def strings_ (sel: A => Any) = new Pes_[String ](sel(allSelections[A])) }
  trait HasSelPesInt    [A, Ignored] { def ints_    (sel: A => Any) = new Pes_[Int    ](sel(allSelections[A])) }
  trait HasSelPesDouble [A, Ignored] { def doubles_ (sel: A => Any) = new Pes_[Double ](sel(allSelections[A])) }
  trait HasSelPesBoolean[A, Ignored] { def booleans_(sel: A => Any) = new Pes_[Boolean](sel(allSelections[A])) }

  trait HasSelPesByte [A, Ignored] { def bytes_ (sel: A => Any) = new Pes_[Byte ](sel(allSelections[A])) }
  trait HasSelPesShort[A, Ignored] { def shorts_(sel: A => Any) = new Pes_[Short](sel(allSelections[A])) }
  trait HasSelPesLong [A, Ignored] { def longs_ (sel: A => Any) = new Pes_[Long] (sel(allSelections[A])) }
  trait HasSelPesFloat[A, Ignored] { def floats_(sel: A => Any) = new Pes_[Float](sel(allSelections[A])) }
  
  trait HasSelPesBigInt[A, Ignored] { def bigInts_(sel: A => Any) = new Pes_[BigInt](sel(allSelections[A])) }
  trait HasSelPesBigDec[A, Ignored] { def bigDecs_(sel: A => Any) = new Pes_[BigDec](sel(allSelections[A])) }

  trait HasSelPesLocalDate     [A, Ignored] { def localDates_     (sel: A => Any) = new Pes_[LocalDate]     (sel(allSelections[A])) }
  trait HasSelPesLocalTime     [A, Ignored] { def localTimes_     (sel: A => Any) = new Pes_[LocalTime]     (sel(allSelections[A])) }
  trait HasSelPesLocalDateTime [A, Ignored] { def localDateTimes_ (sel: A => Any) = new Pes_[LocalDateTime] (sel(allSelections[A])) }
  trait HasSelPesOffsetDateTime[A, Ignored] { def offsetDateTimes_(sel: A => Any) = new Pes_[OffsetDateTime](sel(allSelections[A])) }
  trait HasSelPesZonedDateTime [A, Ignored] { def zonedDateTimes_ (sel: A => Any) = new Pes_[ZonedDateTime] (sel(allSelections[A])) }
  trait HasSelPesInstant       [A, Ignored] { def instants_       (sel: A => Any) = new Pes_[Instant]       (sel(allSelections[A])) }
  
  trait HasSelPesBinary        [A, Ignored] { def binarys_        (sel: A => Any) = new Pes_[ByteBuffer]    (sel(allSelections[A])) }

  // ===========================================================================
  trait HasSelXString [A, Ignored] { def stringx (sel: A => Any) = new One_[String ](sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXInt    [A, Ignored] { def intx    (sel: A => Any) = new One_[Int    ](sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXDouble [A, Ignored] { def doublex (sel: A => Any) = new One_[Double ](sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXBoolean[A, Ignored] { def booleanx(sel: A => Any) = new One_[Boolean](sel(allSelections[A]), ignoreContainer = true) }

  trait HasSelXByte [A, Ignored] { def bytex (sel: A => Any) = new One_[Byte ](sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXShort[A, Ignored] { def shortx(sel: A => Any) = new One_[Short](sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXLong [A, Ignored] { def longx (sel: A => Any) = new One_[Long] (sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXFloat[A, Ignored] { def floatx(sel: A => Any) = new One_[Float](sel(allSelections[A]), ignoreContainer = true) }
  
  trait HasSelXBigInt[A, Ignored] { def bigIntx(sel: A => Any) = new One_[BigInt](sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXBigDec[A, Ignored] { def bigDecx(sel: A => Any) = new One_[BigDec](sel(allSelections[A]), ignoreContainer = true) }      

  trait HasSelXLocalDate     [A, Ignored] { def localDatex     (sel: A => Any) = new One_[LocalDate]     (sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXLocalTime     [A, Ignored] { def localTimex     (sel: A => Any) = new One_[LocalTime]     (sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXLocalDateTime [A, Ignored] { def localDateTimex (sel: A => Any) = new One_[LocalDateTime] (sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXOffsetDateTime[A, Ignored] { def offsetDateTimex(sel: A => Any) = new One_[OffsetDateTime](sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXZonedDateTime [A, Ignored] { def zonedDateTimex (sel: A => Any) = new One_[ZonedDateTime] (sel(allSelections[A]), ignoreContainer = true) }
  trait HasSelXInstant       [A, Ignored] { def instantx       (sel: A => Any) = new One_[Instant]       (sel(allSelections[A]), ignoreContainer = true) }
  
  trait HasSelXBinary        [A, Ignored] { def binaryx        (sel: A => Any) = new One_[ByteBuffer]    (sel(allSelections[A]), ignoreContainer = true) }
}

// ===========================================================================
