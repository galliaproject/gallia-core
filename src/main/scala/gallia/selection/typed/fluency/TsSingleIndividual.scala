package gallia
package selection.typed.fluency

// ===========================================================================
@gallia.TypeMatching // very boilerplatey; TODO: t210124092716 - codegen (very boilerplaty)
object TsSingleIndividual {
  //TODO: enum - t210201095414

  trait HasSingleOneTyped[$Wrap] { def typed[T: WTT](target: $Wrap) = new One_[T](target) } // TODO: t210201103739 - validate T for .typed

    trait HasSingleOneString [$Wrap] { def string (target: $Wrap) = new One_[String ](target) }
    trait HasSingleOneInt    [$Wrap] { def int    (target: $Wrap) = new One_[Int    ](target) }
    trait HasSingleOneDouble [$Wrap] { def double (target: $Wrap) = new One_[Double ](target) }
    trait HasSingleOneBoolean[$Wrap] { def boolean(target: $Wrap) = new One_[Boolean](target) }

    trait HasSingleOneByte [$Wrap] { def byte (target: $Wrap) = new One_[Byte] (target) }
    trait HasSingleOneShort[$Wrap] { def short(target: $Wrap) = new One_[Short](target) }
    trait HasSingleOneLong [$Wrap] { def long (target: $Wrap) = new One_[Long] (target) }

    trait HasSingleOneFloat[$Wrap] { def float(target: $Wrap) = new One_[Float](target) }

    trait HasSingleOneBigInt    [$Wrap] { def bigInt    (target: $Wrap) = new One_[BigInt] (target) }
    trait HasSingleOneBigDecimal[$Wrap] { def bigDecimal(target: $Wrap) = new One_[BigDecimal](target) }

    trait HasSingleOneLocalDate    [$Wrap] { def date    (target: $Wrap) = new One_[LocalDate] (target) }
    trait HasSingleOneLocalDateTime[$Wrap] { def dateTime(target: $Wrap) = new One_[LocalDateTime](target) }

  // ---------------------------------------------------------------------------
  trait HasSingleOptTyped[$Wrap] { def typed_ [T: WTT](target: $Wrap) = new Opt_[T](target) }

    trait HasSingleOptString [$Wrap] { def string_ (target: $Wrap) = new Opt_[String ](target) }
    trait HasSingleOptInt    [$Wrap] { def int_    (target: $Wrap) = new Opt_[Int    ](target) }
    trait HasSingleOptDouble [$Wrap] { def double_ (target: $Wrap) = new Opt_[Double ](target) }
    trait HasSingleOptBoolean[$Wrap] { def boolean_(target: $Wrap) = new Opt_[Boolean](target) }

    trait HasSingleOptByte [$Wrap] { def byte_ (target: $Wrap) = new Opt_[Byte ](target) }
    trait HasSingleOptShort[$Wrap] { def short_(target: $Wrap) = new Opt_[Short](target) }
    trait HasSingleOptLong [$Wrap] { def long_ (target: $Wrap) = new Opt_[Long](target) }

    trait HasSingleOptFloat[$Wrap] { def float_(target: $Wrap) = new Opt_[Float](target) }

    trait HasSingleOptBigInt    [$Wrap] { def bigInt_    (target: $Wrap) = new Opt_[BigInt]    (target) }
    trait HasSingleOptBigDecimal[$Wrap] { def bigDecimal_(target: $Wrap) = new Opt_[BigDecimal](target) }

    trait HasSingleOptLocalDate    [$Wrap] { def date_    (target: $Wrap) = new Opt_[LocalDate ](target) }
    trait HasSingleOptLocalDateTime[$Wrap] { def dateTime_(target: $Wrap) = new Opt_[LocalDateTime](target) }

  // ---------------------------------------------------------------------------
  trait HasSingleNesTyped[$Wrap] { def typeds [T: WTT](target: $Wrap) = new Nes_[T](target) }

    trait HasSingleNesString [$Wrap] { def strings (target: $Wrap) = new Nes_[String ](target) }
    trait HasSingleNesInt    [$Wrap] { def ints    (target: $Wrap) = new Nes_[Int    ](target) }
    trait HasSingleNesDouble [$Wrap] { def doubles (target: $Wrap) = new Nes_[Double ](target) }
    trait HasSingleNesBoolean[$Wrap] { def booleans(target: $Wrap) = new Nes_[Boolean](target) }

    trait HasSingleNesByte [$Wrap] { def bytes (target: $Wrap) = new Nes_[Byte ](target) }
    trait HasSingleNesShort[$Wrap] { def shorts(target: $Wrap) = new Nes_[Short](target) }
    trait HasSingleNesLong [$Wrap] { def longs (target: $Wrap) = new Nes_[Long ](target) }

    trait HasSingleNesFloat[$Wrap] { def floats(target: $Wrap) = new Nes_[Float](target) }

    trait HasSingleNesBigInt    [$Wrap] { def bigInts    (target: $Wrap) = new Nes_[BigInt]    (target) }
    trait HasSingleNesBigDecimal[$Wrap] { def bigDecimals(target: $Wrap) = new Nes_[BigDecimal](target) }

    trait HasSingleNesLocalDate    [$Wrap] { def dates    (target: $Wrap) = new Nes_[LocalDate]    (target) }
    trait HasSingleNesLocalDateTime[$Wrap] { def dateTimes(target: $Wrap) = new Nes_[LocalDateTime](target) }

  // ---------------------------------------------------------------------------
  trait HasSinglePesTyped[$Wrap] { def typeds_[T: WTT](target: $Wrap) = new Pes_[T](target) }

    trait HasSinglePesString [$Wrap] { def strings_ (target: $Wrap) = new Pes_[String ](target) }
    trait HasSinglePesInt    [$Wrap] { def ints_    (target: $Wrap) = new Pes_[Int    ](target) }
    trait HasSinglePesDouble [$Wrap] { def doubles_ (target: $Wrap) = new Pes_[Double ](target) }
    trait HasSinglePesBoolean[$Wrap] { def booleans_(target: $Wrap) = new Pes_[Boolean](target) }

    trait HasSinglePesByte [$Wrap] { def bytes_ (target: $Wrap) = new Pes_[Byte ](target) }
    trait HasSinglePesShort[$Wrap] { def shorts_(target: $Wrap) = new Pes_[Short](target) }
    trait HasSinglePesLong [$Wrap] { def longs_ (target: $Wrap) = new Pes_[Long ](target) }

    trait HasSinglePesFloat[$Wrap] { def floats_(target: $Wrap) = new Pes_[Float](target) }

    trait HasSinglePesBigInt    [$Wrap] { def bigInts_    (target: $Wrap) = new Pes_[BigInt]    (target) }
    trait HasSinglePesBigDecimal[$Wrap] { def bigDecimals_(target: $Wrap) = new Pes_[BigDecimal](target) }

    trait HasSinglePesLocalDate    [$Wrap] { def dates_    (target: $Wrap) = new Pes_[LocalDate]    (target) }
    trait HasSinglePesLocalDateTime[$Wrap] { def dateTimes_(target: $Wrap) = new Pes_[LocalDateTime](target) }

  // ===========================================================================
  trait HasSingleXTyped[$Wrap] { def typed4x[T: WTT](target: $Wrap) = new One_[T](target, ignoreContainer = true) }

    trait HasSingleXString [$Wrap] { def stringx (target: $Wrap ) = new One_[String ](target, ignoreContainer = true) }
    trait HasSingleXInt    [$Wrap] { def intx    (target: $Wrap ) = new One_[Int    ](target, ignoreContainer = true) }
    trait HasSingleXDouble [$Wrap] { def doublex (target: $Wrap ) = new One_[Double ](target, ignoreContainer = true) }
    trait HasSingleXBoolean[$Wrap] { def booleanx(target: $Wrap ) = new One_[Boolean](target, ignoreContainer = true) }

    trait HasSingleXByte [$Wrap] { def bytex (target: $Wrap ) = new One_[Byte ](target, ignoreContainer = true) }
    trait HasSingleXShort[$Wrap] { def shortx(target: $Wrap ) = new One_[Short](target, ignoreContainer = true) }
    trait HasSingleXLong [$Wrap] { def longx (target: $Wrap ) = new One_[Long ](target, ignoreContainer = true) }

    trait HasSingleXFloat[$Wrap] { def floatx(target: $Wrap ) = new One_[Float](target, ignoreContainer = true) }

    trait HasSingleXBigInt    [$Wrap] { def bigIntx    (target: $Wrap ) = new One_[BigInt]    (target, ignoreContainer = true) }
    trait HasSingleXBigDecimal[$Wrap] { def bigDecimalx(target: $Wrap ) = new One_[BigDecimal](target, ignoreContainer = true) }

    trait HasSingleXLocalDate    [$Wrap] { def datex    (target: $Wrap ) = new One_[LocalDate]    (target, ignoreContainer = true) }
    trait HasSingleXLocalDateTime[$Wrap] { def dateTimex(target: $Wrap ) = new One_[LocalDateTime](target, ignoreContainer = true) }

    // ---------------------------------------------------------------------------
    trait HasSingleXEnum[$Wrap] { def enumx[E <: enumeratum.EnumEntry : WTT](target: $Wrap ) = new One_[E](target, ignoreContainer = true) } //FIXME? - try - t210201095414
}

// ===========================================================================
