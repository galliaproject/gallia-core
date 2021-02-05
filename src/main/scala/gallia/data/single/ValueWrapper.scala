package gallia.data.single

// ===========================================================================
@gallia.TypeMatching
class ValueWrapper(val underlying: AnyValue) extends AnyVal {
    //TODO: missing types
    def asTyped[T]: T = underlying.asInstanceOf[T]

    // ---------------------------------------------------------------------------
    def asSeq    : Seq[_]  = asTyped[Seq[_]]

    def asString : String  = asTyped[String]
    def asInt    : Int     = asTyped[Int]
    def asDouble : Double  = asTyped[Double]
    def asBoolean: Boolean = asTyped[Boolean]

    def asByte : Byte  = asTyped[Byte]
    def asShort: Short = asTyped[Short]
    def asLong : Long  = asTyped[Long]

    def asFloat: Float = asTyped[Float]

    def asBigInt       : BigInt     = asTyped[BigInt]
    def asBigDecimal   : BigDecimal = asTyped[BigDecimal]

    def asLocalDate    : LocalDate     = asTyped[LocalDate]
    def asLocalDateTime: LocalDateTime = asTyped[LocalDateTime]

    // ---------------------------------------------------------------------------
    def asObj    : Obj     = asTyped[Obj]
  }

  // ===========================================================================
  object ValueWrapper {
    implicit def to(x: AnyValue): ValueWrapper = new ValueWrapper(x)
  }

// ===========================================================================
