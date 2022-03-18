package gallia
package selection.typed.fluency

// ===========================================================================
@TypeMatching // very boilerplatey; TODO: t210124092716 - codegen (very boilerplaty)
object TsRepeatedIndividual { //TODO: enum - t210201095414
  
  // ---------------------------------------------------------------------------  
  trait HasRepeatedNonXTyped[$Wrap, $Wrapz <: Seq[$Wrap]] { def typed [T: WTT](x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[T]((x1, x2, more)) } // TODO: t210201103739 - validate T for .typed
  trait HasRepeatedXTyped   [$Wrap, $Wrapz <: Seq[$Wrap]] { def typedx[T: WTT](x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[T]((x1, x2, more), ignoreContainer = true) }

  // ===========================================================================
  trait HasRepeatedOneString [$Wrap, $Wrapz <: Seq[$Wrap]] { def string (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[String] ((x1, x2, more)) }
  trait HasRepeatedOneInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def int    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Int]    ((x1, x2, more)) }
  trait HasRepeatedOneDouble [$Wrap, $Wrapz <: Seq[$Wrap]] { def double (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Double] ((x1, x2, more)) }
  trait HasRepeatedOneBoolean[$Wrap, $Wrapz <: Seq[$Wrap]] { def boolean(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Boolean]((x1, x2, more)) }

  trait HasRepeatedOneByte [$Wrap, $Wrapz <: Seq[$Wrap]] { def byte (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Byte] ((x1, x2, more)) }
  trait HasRepeatedOneShort[$Wrap, $Wrapz <: Seq[$Wrap]] { def short(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Short]((x1, x2, more)) }
  trait HasRepeatedOneLong [$Wrap, $Wrapz <: Seq[$Wrap]] { def long (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Long] ((x1, x2, more)) }
  trait HasRepeatedOneFloat[$Wrap, $Wrapz <: Seq[$Wrap]] { def float(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Float]((x1, x2, more)) }

  trait HasRepeatedOneBigInt[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigInt    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[BigInt]((x1, x2, more)) }
  trait HasRepeatedOneBigDec[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigDecimal(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[BigDec]((x1, x2, more)) }

  trait HasRepeatedOneLocalDate     [$Wrap, $Wrapz <: Seq[$Wrap]] { def localDate     (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[LocalDate]     ((x1, x2, more)) }
  trait HasRepeatedOneLocalTime     [$Wrap, $Wrapz <: Seq[$Wrap]] { def localTime     (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[LocalTime]     ((x1, x2, more)) }    
  trait HasRepeatedOneLocalDateTime [$Wrap, $Wrapz <: Seq[$Wrap]] { def localDateTime (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[LocalDateTime] ((x1, x2, more)) }
  trait HasRepeatedOneOffsetDateTime[$Wrap, $Wrapz <: Seq[$Wrap]] { def offsetDateTime(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[OffsetDateTime]((x1, x2, more)) }
  trait HasRepeatedOneZonedDateTime [$Wrap, $Wrapz <: Seq[$Wrap]] { def zonedDateTime (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[ZonedDateTime] ((x1, x2, more)) }
  trait HasRepeatedOneInstant       [$Wrap, $Wrapz <: Seq[$Wrap]] { def instant       (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Instant]       ((x1, x2, more)) }            
  
  // ===========================================================================
  trait HasRepeatedOptString [$Wrap, $Wrapz <: Seq[$Wrap]] { def string_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[String] ((x1, x2, more)) }
  trait HasRepeatedOptInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def int_    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Int]    ((x1, x2, more)) }
  trait HasRepeatedOptDouble [$Wrap, $Wrapz <: Seq[$Wrap]] { def double_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Double] ((x1, x2, more)) }
  trait HasRepeatedOptBoolean[$Wrap, $Wrapz <: Seq[$Wrap]] { def boolean_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Boolean]((x1, x2, more)) }

  trait HasRepeatedOptByte [$Wrap, $Wrapz <: Seq[$Wrap]] { def byte_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Byte] ((x1, x2, more)) }
  trait HasRepeatedOptShort[$Wrap, $Wrapz <: Seq[$Wrap]] { def short_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Short]((x1, x2, more)) }
  trait HasRepeatedOptLong [$Wrap, $Wrapz <: Seq[$Wrap]] { def long_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Long] ((x1, x2, more)) }
  trait HasRepeatedOptFloat[$Wrap, $Wrapz <: Seq[$Wrap]] { def float_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Float]((x1, x2, more)) }

  trait HasRepeatedOptBigInt[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigInt_    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[BigInt]((x1, x2, more)) }
  trait HasRepeatedOptBigDec[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigDecimal_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[BigDec]((x1, x2, more)) }
  
  trait HasRepeatedOptLocalDate     [$Wrap, $Wrapz <: Seq[$Wrap]] { def localDate_     (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[LocalDate]     ((x1, x2, more)) }
  trait HasRepeatedOptLocalTime     [$Wrap, $Wrapz <: Seq[$Wrap]] { def localTime_     (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[LocalTime]     ((x1, x2, more)) }    
  trait HasRepeatedOptLocalDateTime [$Wrap, $Wrapz <: Seq[$Wrap]] { def localDateTime_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[LocalDateTime] ((x1, x2, more)) }
  trait HasRepeatedOptOffsetDateTime[$Wrap, $Wrapz <: Seq[$Wrap]] { def offsetDateTime_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[OffsetDateTime]((x1, x2, more)) }
  trait HasRepeatedOptZonedDateTime [$Wrap, $Wrapz <: Seq[$Wrap]] { def zonedDateTime_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[ZonedDateTime] ((x1, x2, more)) }
  trait HasRepeatedOptInstant       [$Wrap, $Wrapz <: Seq[$Wrap]] { def instant_       (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Instant]       ((x1, x2, more)) }
     
  // ===========================================================================
  trait HasRepeatedNesString [$Wrap, $Wrapz <: Seq[$Wrap]] { def strings (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[String] ((x1, x2, more)) }
  trait HasRepeatedNesInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def ints    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Int]    ((x1, x2, more)) }
  trait HasRepeatedNesDouble [$Wrap, $Wrapz <: Seq[$Wrap]] { def doubles (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Double] ((x1, x2, more)) }
  trait HasRepeatedNesBoolean[$Wrap, $Wrapz <: Seq[$Wrap]] { def booleans(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Boolean]((x1, x2, more)) }

  trait HasRepeatedNesByte [$Wrap, $Wrapz <: Seq[$Wrap]] { def bytes (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Byte] ((x1, x2, more)) }
  trait HasRepeatedNesShort[$Wrap, $Wrapz <: Seq[$Wrap]] { def shorts(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Short]((x1, x2, more)) }
  trait HasRepeatedNesLong [$Wrap, $Wrapz <: Seq[$Wrap]] { def longs (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Long] ((x1, x2, more)) }
  trait HasRepeatedNesFloat[$Wrap, $Wrapz <: Seq[$Wrap]] { def floats(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Float]((x1, x2, more)) }

  trait HasRepeatedNesBigInt[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigInts    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[BigInt]((x1, x2, more)) }
  trait HasRepeatedNesBigDec[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigDecimals(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[BigDec]((x1, x2, more)) }

  trait HasRepeatedNesLocalDate     [$Wrap, $Wrapz <: Seq[$Wrap]] { def localDates     (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[LocalDate]     ((x1, x2, more)) }
  trait HasRepeatedNesLocalTime     [$Wrap, $Wrapz <: Seq[$Wrap]] { def localTimes     (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[LocalTime]     ((x1, x2, more)) }    
  trait HasRepeatedNesLocalDateTime [$Wrap, $Wrapz <: Seq[$Wrap]] { def localDateTimes (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[LocalDateTime] ((x1, x2, more)) }
  trait HasRepeatedNesOffsetDateTime[$Wrap, $Wrapz <: Seq[$Wrap]] { def offsetDateTimes(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[OffsetDateTime]((x1, x2, more)) }
  trait HasRepeatedNesZonedDateTime [$Wrap, $Wrapz <: Seq[$Wrap]] { def zonedDateTimes (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[ZonedDateTime] ((x1, x2, more)) }
  trait HasRepeatedNesInstant       [$Wrap, $Wrapz <: Seq[$Wrap]] { def instants       (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Instant]       ((x1, x2, more)) }

  // ===========================================================================
  trait HasRepeatedPesString [$Wrap, $Wrapz <: Seq[$Wrap]] { def strings_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[String] ((x1, x2, more)) }
  trait HasRepeatedPesInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def ints_    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Int]    ((x1, x2, more)) }
  trait HasRepeatedPesDouble [$Wrap, $Wrapz <: Seq[$Wrap]] { def doubles_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Double] ((x1, x2, more)) }
  trait HasRepeatedPesBoolean[$Wrap, $Wrapz <: Seq[$Wrap]] { def booleans_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Boolean]((x1, x2, more)) }

  trait HasRepeatedPesByte [$Wrap, $Wrapz <: Seq[$Wrap]] { def bytes_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Byte] ((x1, x2, more)) }
  trait HasRepeatedPesShort[$Wrap, $Wrapz <: Seq[$Wrap]] { def shorts_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Short]((x1, x2, more)) }
  trait HasRepeatedPesLong [$Wrap, $Wrapz <: Seq[$Wrap]] { def longs_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Long] ((x1, x2, more)) }
  trait HasRepeatedPesFloat[$Wrap, $Wrapz <: Seq[$Wrap]] { def floats_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Float]((x1, x2, more)) }

  trait HasRepeatedPesBigInt[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigInts_    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[BigInt]((x1, x2, more)) }
  trait HasRepeatedPesBigDec[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigDecimals_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[BigDec]((x1, x2, more)) }

  trait HasRepeatedPesLocalDate     [$Wrap, $Wrapz <: Seq[$Wrap]] { def localDates_     (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[LocalDate]     ((x1, x2, more)) }
  trait HasRepeatedPesLocalTime     [$Wrap, $Wrapz <: Seq[$Wrap]] { def localTimes_     (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[LocalTime]     ((x1, x2, more)) }    
  trait HasRepeatedPesLocalDateTime [$Wrap, $Wrapz <: Seq[$Wrap]] { def localDateTimes_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[LocalDateTime] ((x1, x2, more)) }
  trait HasRepeatedPesOffsetDateTime[$Wrap, $Wrapz <: Seq[$Wrap]] { def offsetDateTimes_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[OffsetDateTime]((x1, x2, more)) }
  trait HasRepeatedPesZonedDateTime [$Wrap, $Wrapz <: Seq[$Wrap]] { def zonedDateTimes_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[ZonedDateTime] ((x1, x2, more)) }
  trait HasRepeatedPesInstant       [$Wrap, $Wrapz <: Seq[$Wrap]] { def instants_       (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Instant]       ((x1, x2, more)) }

  // ===========================================================================
  trait HasRepeatedXString [$Wrap, $Wrapz <: Seq[$Wrap]] { def stringx (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[String] ((x1, x2, more), ignoreContainer = true) }
  trait HasRepeatedXInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def intx    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Int]    ((x1, x2, more), ignoreContainer = true) }
  trait HasRepeatedXDouble [$Wrap, $Wrapz <: Seq[$Wrap]] { def doublex (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Double] ((x1, x2, more), ignoreContainer = true) }
  trait HasRepeatedXBoolean[$Wrap, $Wrapz <: Seq[$Wrap]] { def booleanx(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Boolean]((x1, x2, more), ignoreContainer = true) }

  trait HasRepeatedXByte [$Wrap, $Wrapz <: Seq[$Wrap]] { def bytex (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Byte] ((x1, x2, more), ignoreContainer = true) }
  trait HasRepeatedXShort[$Wrap, $Wrapz <: Seq[$Wrap]] { def shortx(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Short]((x1, x2, more), ignoreContainer = true) }
  trait HasRepeatedXLong [$Wrap, $Wrapz <: Seq[$Wrap]] { def longx (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Long] ((x1, x2, more), ignoreContainer = true) }
  trait HasRepeatedXFloat[$Wrap, $Wrapz <: Seq[$Wrap]] { def floatx(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Float]((x1, x2, more), ignoreContainer = true) }

  trait HasRepeatedXBigInt[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigIntx    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[BigInt]((x1, x2, more), ignoreContainer = true) }
  trait HasRepeatedXBigDec[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigDecimalx(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[BigDec]((x1, x2, more), ignoreContainer = true) }

  trait HasRepeatedXLocalDate     [$Wrap, $Wrapz <: Seq[$Wrap]] { def localDatex     (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[LocalDate]     ((x1, x2, more), ignoreContainer = true) }
  trait HasRepeatedXLocalTime     [$Wrap, $Wrapz <: Seq[$Wrap]] { def localTimex     (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[LocalTime]     ((x1, x2, more), ignoreContainer = true) }    
  trait HasRepeatedXLocalDateTime [$Wrap, $Wrapz <: Seq[$Wrap]] { def localDateTimex (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[LocalDateTime] ((x1, x2, more), ignoreContainer = true) }
  trait HasRepeatedXOffsetDateTime[$Wrap, $Wrapz <: Seq[$Wrap]] { def offsetDateTimex(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[OffsetDateTime]((x1, x2, more), ignoreContainer = true) }
  trait HasRepeatedXZonedDateTime [$Wrap, $Wrapz <: Seq[$Wrap]] { def zonedDateTimex (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[ZonedDateTime] ((x1, x2, more), ignoreContainer = true) }
  trait HasRepeatedXInstant       [$Wrap, $Wrapz <: Seq[$Wrap]] { def instantx       (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Instant]       ((x1, x2, more), ignoreContainer = true) }

}

// ===========================================================================
