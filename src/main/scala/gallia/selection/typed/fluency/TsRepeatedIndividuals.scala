package gallia
package selection.typed.fluency

// ===========================================================================
@gallia.TypeMatching // very boilerplatey; TODO: t210124092716 - codegen (very boilerplaty)
object TsRepeatedIndividuals {

  // ---------------------------------------------------------------------------
  trait HasRepeatedOneTyped [$Wrap, $Wrapz <: Seq[$Wrap]] { def typed[T: WTT](x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[T]((x1, x2, more)) } // TODO: t210201103739 - validate T for .typed

    trait HasRepeatedOneString [$Wrap, $Wrapz <: Seq[$Wrap]] { def string (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[String] ((x1, x2, more)) }
    trait HasRepeatedOneInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def int    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Int]    ((x1, x2, more)) }
    trait HasRepeatedOneDouble [$Wrap, $Wrapz <: Seq[$Wrap]] { def double (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Double] ((x1, x2, more)) }
    trait HasRepeatedOneBoolean[$Wrap, $Wrapz <: Seq[$Wrap]] { def boolean(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Boolean]((x1, x2, more)) }

    trait HasRepeatedOneByte [$Wrap, $Wrapz <: Seq[$Wrap]] { def byte (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Byte] ((x1, x2, more)) }
    trait HasRepeatedOneShort[$Wrap, $Wrapz <: Seq[$Wrap]] { def short(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Short]((x1, x2, more)) }
    trait HasRepeatedOneLong [$Wrap, $Wrapz <: Seq[$Wrap]] { def long (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Long] ((x1, x2, more)) }

    trait HasRepeatedOneFloat[$Wrap, $Wrapz <: Seq[$Wrap]] { def float(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Float]((x1, x2, more)) }

    trait HasRepeatedOneBigInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def bigInt    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[BigInt]    ((x1, x2, more)) }
    trait HasRepeatedOneBigDecimal[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigDecimal(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[BigDecimal]((x1, x2, more)) }

    trait HasRepeatedOneLocalDate    [$Wrap, $Wrapz <: Seq[$Wrap]] { def date    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[LocalDate]    ((x1, x2, more)) }
    trait HasRepeatedOneLocalDateTime[$Wrap, $Wrapz <: Seq[$Wrap]] { def dateTime(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[LocalDateTime]((x1, x2, more)) }

  // ===========================================================================
  trait HasRepeatedOptTyped [$Wrap, $Wrapz <: Seq[$Wrap]] { def typed_[T: WTT](x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[T]((x1, x2, more)) }

    trait HasRepeatedOptString [$Wrap, $Wrapz <: Seq[$Wrap]] { def string_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[String]((x1, x2, more)) }
    trait HasRepeatedOptIn     [$Wrap, $Wrapz <: Seq[$Wrap]] { def int_    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Int]((x1, x2, more)) }
    trait HasRepeatedOptDouble [$Wrap, $Wrapz <: Seq[$Wrap]] { def double_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Double]((x1, x2, more)) }
    trait HasRepeatedOptBoolean[$Wrap, $Wrapz <: Seq[$Wrap]] { def boolean_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Boolean]((x1, x2, more)) }

    trait HasRepeatedOptByte [$Wrap, $Wrapz <: Seq[$Wrap]] { def byte (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Byte] ((x1, x2, more)) }
    trait HasRepeatedOptShort[$Wrap, $Wrapz <: Seq[$Wrap]] { def short(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Short]((x1, x2, more)) }
    trait HasRepeatedOptLong [$Wrap, $Wrapz <: Seq[$Wrap]] { def long (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Long] ((x1, x2, more)) }

    trait HasRepeatedOptFloat[$Wrap, $Wrapz <: Seq[$Wrap]] { def float(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[Float]((x1, x2, more)) }

    trait HasRepeatedOptBigInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def bigInt    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[BigInt]    ((x1, x2, more)) }
    trait HasRepeatedOptBigDecimal[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigDecimal(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[BigDecimal]((x1, x2, more)) }

    trait HasRepeatedOptLocalDate    [$Wrap, $Wrapz <: Seq[$Wrap]] { def date    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[LocalDate]    ((x1, x2, more)) }
    trait HasRepeatedOptLocalDateTime[$Wrap, $Wrapz <: Seq[$Wrap]] { def dateTime(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Opt_[LocalDateTime]((x1, x2, more)) }

  // ===========================================================================
  trait HasRepeatedNesTyped [$Wrap, $Wrapz <: Seq[$Wrap]] { def typeds[T: WTT](x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[T]((x1, x2, more)) }

    trait HasRepeatedNesString [$Wrap, $Wrapz <: Seq[$Wrap]] { def strings (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[String]((x1, x2, more)) }
    trait HasRepeatedNesInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def ints    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Int]((x1, x2, more)) }
    trait HasRepeatedNesDouble [$Wrap, $Wrapz <: Seq[$Wrap]] { def doubles (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Double]((x1, x2, more)) }
    trait HasRepeatedNesBoolean[$Wrap, $Wrapz <: Seq[$Wrap]] { def booleans(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Boolean]((x1, x2, more)) }

    trait HasRepeatedNesByte [$Wrap, $Wrapz <: Seq[$Wrap]] { def byte (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Byte] ((x1, x2, more)) }
    trait HasRepeatedNesShort[$Wrap, $Wrapz <: Seq[$Wrap]] { def short(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Short]((x1, x2, more)) }
    trait HasRepeatedNesLong [$Wrap, $Wrapz <: Seq[$Wrap]] { def long (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Long] ((x1, x2, more)) }

    trait HasRepeatedNesFloat[$Wrap, $Wrapz <: Seq[$Wrap]] { def float(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[Float]((x1, x2, more)) }

    trait HasRepeatedNesBigInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def bigInt    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[BigInt]    ((x1, x2, more)) }
    trait HasRepeatedNesBigDecimal[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigDecimal(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[BigDecimal]((x1, x2, more)) }

    trait HasRepeatedNesLocalDate    [$Wrap, $Wrapz <: Seq[$Wrap]] { def date    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[LocalDate]    ((x1, x2, more)) }
    trait HasRepeatedNesLocalDateTime[$Wrap, $Wrapz <: Seq[$Wrap]] { def dateTime(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Nes_[LocalDateTime]((x1, x2, more)) }

  // ===========================================================================
  trait HasRepeatedPesTyped [$Wrap, $Wrapz <: Seq[$Wrap]] { def typeds_[T: WTT](x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[T]((x1, x2, more)) }

    trait HasRepeatedPesString [$Wrap, $Wrapz <: Seq[$Wrap]] { def strings_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[String]((x1, x2, more)) }
    trait HasRepeatedPesInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def ints_    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Int]((x1, x2, more)) }
    trait HasRepeatedPesDouble [$Wrap, $Wrapz <: Seq[$Wrap]] { def doubles_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Double]((x1, x2, more)) }
    trait HasRepeatedPesBoolean[$Wrap, $Wrapz <: Seq[$Wrap]] { def booleans_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Boolean]((x1, x2, more)) }

    trait HasRepeatedPesByte [$Wrap, $Wrapz <: Seq[$Wrap]] { def byte (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Byte] ((x1, x2, more)) }
    trait HasRepeatedPesShort[$Wrap, $Wrapz <: Seq[$Wrap]] { def short(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Short]((x1, x2, more)) }
    trait HasRepeatedPesLong [$Wrap, $Wrapz <: Seq[$Wrap]] { def long (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Long] ((x1, x2, more)) }

    trait HasRepeatedPesFloat[$Wrap, $Wrapz <: Seq[$Wrap]] { def float(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[Float]((x1, x2, more)) }

    trait HasRepeatedPesBigInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def bigInt    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[BigInt]    ((x1, x2, more)) }
    trait HasRepeatedPesBigDecimal[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigDecimal(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[BigDecimal]((x1, x2, more)) }

    trait HasRepeatedPesLocalDate    [$Wrap, $Wrapz <: Seq[$Wrap]] { def date    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[LocalDate]    ((x1, x2, more)) }
    trait HasRepeatedPesLocalDateTime[$Wrap, $Wrapz <: Seq[$Wrap]] { def dateTime(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new Pes_[LocalDateTime]((x1, x2, more)) }

  // ===========================================================================
  trait HasRepeatedXTyped [$Wrap, $Wrapz <: Seq[$Wrap]] { def typeds_[T: WTT](x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[T]((x1, x2, more), ignoreContainer = true) }

    trait HasRepeatedXString [$Wrap, $Wrapz <: Seq[$Wrap]] { def strings_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[String]((x1, x2, more), ignoreContainer = true) }
    trait HasRepeatedXInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def ints_    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Int]((x1, x2, more), ignoreContainer = true) }
    trait HasRepeatedXDouble [$Wrap, $Wrapz <: Seq[$Wrap]] { def doubles_ (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Double]((x1, x2, more), ignoreContainer = true) }
    trait HasRepeatedXBoolean[$Wrap, $Wrapz <: Seq[$Wrap]] { def booleans_(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Boolean]((x1, x2, more), ignoreContainer = true) }

    trait HasRepeatedXByte [$Wrap, $Wrapz <: Seq[$Wrap]] { def byte (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Byte] ((x1, x2, more), ignoreContainer = true) }
    trait HasRepeatedXShort[$Wrap, $Wrapz <: Seq[$Wrap]] { def short(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Short]((x1, x2, more), ignoreContainer = true) }
    trait HasRepeatedXLong [$Wrap, $Wrapz <: Seq[$Wrap]] { def long (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Long] ((x1, x2, more), ignoreContainer = true) }

    trait HasRepeatedXFloat[$Wrap, $Wrapz <: Seq[$Wrap]] { def float(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[Float]((x1, x2, more), ignoreContainer = true) }

    trait HasRepeatedXBigInt    [$Wrap, $Wrapz <: Seq[$Wrap]] { def bigInt    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[BigInt]    ((x1, x2, more), ignoreContainer = true) }
    trait HasRepeatedXBigDecimal[$Wrap, $Wrapz <: Seq[$Wrap]] { def bigDecimal(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[BigDecimal]((x1, x2, more), ignoreContainer = true) }

    trait HasRepeatedXLocalDate    [$Wrap, $Wrapz <: Seq[$Wrap]] { def date    (x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[LocalDate]    ((x1, x2, more), ignoreContainer = true) }
    trait HasRepeatedXLocalDateTime[$Wrap, $Wrapz <: Seq[$Wrap]] { def dateTime(x1: $Wrap, x2: $Wrap, more: $Wrap*) = new One_[LocalDateTime]((x1, x2, more), ignoreContainer = true) }

}

// ===========================================================================
