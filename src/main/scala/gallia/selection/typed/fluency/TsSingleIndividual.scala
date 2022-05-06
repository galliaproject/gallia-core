package gallia
package selection.typed.fluency

// ===========================================================================
@TypeMatching // very boilerplatey; TODO: t210124092716 - codegen (very boilerplaty)
object TsSingleIndividual {

  // ---------------------------------------------------------------------------
  //TODO: rename, these also abstract requiredness
  trait HasSingleObj [$Wrap] { def obj  (target: $Wrap) = new One_[gallia.HeadU](target) }
  trait HasSingleObjz[$Wrap] { def objz (target: $Wrap) = new One_[gallia.HeadZ](target) }
  
  // ===========================================================================  
  trait HasSingleNonXTyped[$Wrap] { def typed [T: WTT](target: $Wrap) = new One_[T](target) } // TODO: t210201103739 - validate T for .typed
  trait HasSingleXTyped   [$Wrap] { def typedx[T: WTT](target: $Wrap) = new One_[T](target, ignoreContainer = true) }
  
  // ===========================================================================
  trait HasSingleOneString [$Wrap] { def string (target: $Wrap) = new One_[String ](target) }
  trait HasSingleOneInt    [$Wrap] { def int    (target: $Wrap) = new One_[Int    ](target) }
  trait HasSingleOneDouble [$Wrap] { def double (target: $Wrap) = new One_[Double ](target) }
  trait HasSingleOneBoolean[$Wrap] { def boolean(target: $Wrap) = new One_[Boolean](target) }

  trait HasSingleOneByte [$Wrap] { def byte (target: $Wrap) = new One_[Byte] (target) }
  trait HasSingleOneShort[$Wrap] { def short(target: $Wrap) = new One_[Short](target) }
  trait HasSingleOneLong [$Wrap] { def long (target: $Wrap) = new One_[Long] (target) }
  trait HasSingleOneFloat[$Wrap] { def float(target: $Wrap) = new One_[Float](target) }

  trait HasSingleOneBigInt[$Wrap] { def bigInt(target: $Wrap) = new One_[BigInt](target) }
  trait HasSingleOneBigDec[$Wrap] { def bigDec(target: $Wrap) = new One_[BigDec](target) }

  trait HasSingleOneLocalDate     [$Wrap] { def localDate     (target: $Wrap) = new One_[LocalDate]     (target) }
  trait HasSingleOneLocalTime     [$Wrap] { def localTime     (target: $Wrap) = new One_[LocalTime]     (target) }
  trait HasSingleOneLocalDateTime [$Wrap] { def localDateTime (target: $Wrap) = new One_[LocalDateTime] (target) }
  trait HasSingleOneOffsetDateTime[$Wrap] { def offsetDateTime(target: $Wrap) = new One_[OffsetDateTime](target) }
  trait HasSingleOneZonedDateTime [$Wrap] { def zonedDateTime (target: $Wrap) = new One_[ZonedDateTime] (target) }
  trait HasSingleOneInstant       [$Wrap] { def instant       (target: $Wrap) = new One_[Instant]       (target) }
  
  trait HasSingleOneBinary        [$Wrap] { def binary        (target: $Wrap) = new One_[ByteBuffer]    (target) }

  trait HasSingleOneEnm           [$Wrap] { def enm           (target: $Wrap) = new One_[EnumValue]      (target) }

  // ---------------------------------------------------------------------------
  trait HasSingleOptString [$Wrap] { def string_ (target: $Wrap) = new Opt_[String ](target) }
  trait HasSingleOptInt    [$Wrap] { def int_    (target: $Wrap) = new Opt_[Int    ](target) }
  trait HasSingleOptDouble [$Wrap] { def double_ (target: $Wrap) = new Opt_[Double ](target) }
  trait HasSingleOptBoolean[$Wrap] { def boolean_(target: $Wrap) = new Opt_[Boolean](target) }

  trait HasSingleOptByte [$Wrap] { def byte_ (target: $Wrap) = new Opt_[Byte ](target) }
  trait HasSingleOptShort[$Wrap] { def short_(target: $Wrap) = new Opt_[Short](target) }
  trait HasSingleOptLong [$Wrap] { def long_ (target: $Wrap) = new Opt_[Long](target) }
  trait HasSingleOptFloat[$Wrap] { def float_(target: $Wrap) = new Opt_[Float](target) }

  trait HasSingleOptBigInt[$Wrap] { def bigInt_(target: $Wrap) = new Opt_[BigInt]    (target) }
  trait HasSingleOptBigDec[$Wrap] { def bigDec_(target: $Wrap) = new Opt_[BigDec](target) }

  trait HasSingleOptLocalDate     [$Wrap] { def localDate_     (target: $Wrap) = new Opt_[LocalDate ]    (target) }
  trait HasSingleOptLocalTime     [$Wrap] { def localTime_     (target: $Wrap) = new Opt_[LocalTime ]    (target) }
  trait HasSingleOptLocalDateTime [$Wrap] { def localDateTime_ (target: $Wrap) = new Opt_[LocalDateTime] (target) }
  trait HasSingleOptOffsetDateTime[$Wrap] { def offsetDateTime_(target: $Wrap) = new Opt_[OffsetDateTime](target) }
  trait HasSingleOptZonedDateTime [$Wrap] { def zonedDateTime_ (target: $Wrap) = new Opt_[ZonedDateTime] (target) }
  trait HasSingleOptInstant       [$Wrap] { def instant_       (target: $Wrap) = new Opt_[Instant]       (target) }
  
  trait HasSingleOptBinary        [$Wrap] { def binary_        (target: $Wrap) = new Opt_[ByteBuffer]    (target) }

  trait HasSingleOptEnm           [$Wrap] { def enm_           (target: $Wrap) = new Opt_[EnumValue]      (target) }

  // ---------------------------------------------------------------------------
  trait HasSingleNesString [$Wrap] { def strings (target: $Wrap) = new Nes_[String ](target) }
  trait HasSingleNesInt    [$Wrap] { def ints    (target: $Wrap) = new Nes_[Int    ](target) }
  trait HasSingleNesDouble [$Wrap] { def doubles (target: $Wrap) = new Nes_[Double ](target) }
  trait HasSingleNesBoolean[$Wrap] { def booleans(target: $Wrap) = new Nes_[Boolean](target) }

  trait HasSingleNesByte [$Wrap] { def bytes (target: $Wrap) = new Nes_[Byte ](target) }
  trait HasSingleNesShort[$Wrap] { def shorts(target: $Wrap) = new Nes_[Short](target) }
  trait HasSingleNesLong [$Wrap] { def longs (target: $Wrap) = new Nes_[Long ](target) }
  trait HasSingleNesFloat[$Wrap] { def floats(target: $Wrap) = new Nes_[Float](target) }

  trait HasSingleNesBigInt[$Wrap] { def bigInts(target: $Wrap) = new Nes_[BigInt]    (target) }
  trait HasSingleNesBigDec[$Wrap] { def bigDecs(target: $Wrap) = new Nes_[BigDec](target) }

  trait HasSingleNesLocalDate     [$Wrap] { def localDates     (target: $Wrap) = new Nes_[LocalDate]     (target) }
  trait HasSingleNesLocalTime     [$Wrap] { def localTimes     (target: $Wrap) = new Nes_[LocalTime]     (target) }
  trait HasSingleNesLocalDateTime [$Wrap] { def localDateTimes (target: $Wrap) = new Nes_[LocalDateTime] (target) }
  trait HasSingleNesOffsetDateTime[$Wrap] { def offsetDateTimes(target: $Wrap) = new Nes_[OffsetDateTime](target) }
  trait HasSingleNesZonedDateTime [$Wrap] { def zonedDateTimes (target: $Wrap) = new Nes_[ZonedDateTime] (target) }
  trait HasSingleNesInstant       [$Wrap] { def instants       (target: $Wrap) = new Nes_[Instant]       (target) }
  
  trait HasSingleNesBinary        [$Wrap] { def binarys        (target: $Wrap) = new Nes_[ByteBuffer]    (target) }

  trait HasSingleNesEnm           [$Wrap] { def enms           (target: $Wrap) = new Nes_[EnumValue]      (target) }

  // ---------------------------------------------------------------------------
  trait HasSinglePesString [$Wrap] { def strings_ (target: $Wrap) = new Pes_[String ](target) }
  trait HasSinglePesInt    [$Wrap] { def ints_    (target: $Wrap) = new Pes_[Int    ](target) }
  trait HasSinglePesDouble [$Wrap] { def doubles_ (target: $Wrap) = new Pes_[Double ](target) }
  trait HasSinglePesBoolean[$Wrap] { def booleans_(target: $Wrap) = new Pes_[Boolean](target) }

  trait HasSinglePesByte [$Wrap] { def bytes_ (target: $Wrap) = new Pes_[Byte ](target) }
  trait HasSinglePesShort[$Wrap] { def shorts_(target: $Wrap) = new Pes_[Short](target) }
  trait HasSinglePesLong [$Wrap] { def longs_ (target: $Wrap) = new Pes_[Long ](target) }
  trait HasSinglePesFloat[$Wrap] { def floats_(target: $Wrap) = new Pes_[Float](target) }

  trait HasSinglePesBigInt[$Wrap] { def bigInts_(target: $Wrap) = new Pes_[BigInt]    (target) }
  trait HasSinglePesBigDec[$Wrap] { def bigDecs_(target: $Wrap) = new Pes_[BigDec](target) }

  trait HasSinglePesLocalDate     [$Wrap] { def localDates_     (target: $Wrap) = new Pes_[LocalDate]     (target) }
  trait HasSinglePesLocalTime     [$Wrap] { def localTimes_     (target: $Wrap) = new Pes_[LocalTime]     (target) }
  trait HasSinglePesLocalDateTime [$Wrap] { def localDateTimes_ (target: $Wrap) = new Pes_[LocalDateTime] (target) }
  trait HasSinglePesOffsetDateTime[$Wrap] { def offsetDateTimes_(target: $Wrap) = new Pes_[OffsetDateTime](target) }
  trait HasSinglePesZonedDateTime [$Wrap] { def zonedDateTimes_ (target: $Wrap) = new Pes_[ZonedDateTime] (target) }
  trait HasSinglePesInstant       [$Wrap] { def instants_       (target: $Wrap) = new Pes_[Instant]       (target) }
  
  trait HasSinglePesBinary        [$Wrap] { def binarys_        (target: $Wrap) = new Pes_[ByteBuffer]    (target) }

  trait HasSinglePesEnm           [$Wrap] { def enms_           (target: $Wrap) = new Pes_[EnumValue]      (target) }

  // ===========================================================================
  trait HasSingleXString [$Wrap] { def stringx (target: $Wrap ) = new One_[String ](target, ignoreContainer = true) }
  trait HasSingleXInt    [$Wrap] { def intx    (target: $Wrap ) = new One_[Int    ](target, ignoreContainer = true) }
  trait HasSingleXDouble [$Wrap] { def doublex (target: $Wrap ) = new One_[Double ](target, ignoreContainer = true) }
  trait HasSingleXBoolean[$Wrap] { def booleanx(target: $Wrap ) = new One_[Boolean](target, ignoreContainer = true) }

  trait HasSingleXByte [$Wrap] { def bytex (target: $Wrap ) = new One_[Byte ](target, ignoreContainer = true) }
  trait HasSingleXShort[$Wrap] { def shortx(target: $Wrap ) = new One_[Short](target, ignoreContainer = true) }
  trait HasSingleXLong [$Wrap] { def longx (target: $Wrap ) = new One_[Long ](target, ignoreContainer = true) }
  trait HasSingleXFloat[$Wrap] { def floatx(target: $Wrap ) = new One_[Float](target, ignoreContainer = true) }

  trait HasSingleXBigInt[$Wrap] { def bigIntx(target: $Wrap ) = new One_[BigInt]    (target, ignoreContainer = true) }
  trait HasSingleXBigDec[$Wrap] { def bigDecx(target: $Wrap ) = new One_[BigDec](target, ignoreContainer = true) }

  trait HasSingleXLocalDate     [$Wrap] { def localDatex     (target: $Wrap ) = new One_[LocalDate]     (target, ignoreContainer = true) }
  trait HasSingleXLocalTime     [$Wrap] { def localTimex     (target: $Wrap ) = new One_[LocalTime]     (target, ignoreContainer = true) }
  trait HasSingleXLocalDateTime [$Wrap] { def localDateTimex (target: $Wrap ) = new One_[LocalDateTime] (target, ignoreContainer = true) }
  trait HasSingleXOffsetDateTime[$Wrap] { def offsetDateTimex(target: $Wrap ) = new One_[OffsetDateTime](target, ignoreContainer = true) }
  trait HasSingleXZonedDateTime [$Wrap] { def zonedDateTimex (target: $Wrap ) = new One_[ZonedDateTime] (target, ignoreContainer = true) }
  trait HasSingleXInstant       [$Wrap] { def instantx       (target: $Wrap ) = new One_[Instant]       (target, ignoreContainer = true) }
  
  trait HasSingleXBinary        [$Wrap] { def binaryx        (target: $Wrap ) = new One_[ByteBuffer]    (target, ignoreContainer = true) }

  trait HasSingleXEnm           [$Wrap] { def enmx           (target: $Wrap ) = new One_[EnumValue]      (target, ignoreContainer = true) }
}

// ===========================================================================
