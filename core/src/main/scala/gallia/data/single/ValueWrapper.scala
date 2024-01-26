package gallia
package data
package single

// ===========================================================================
@TypeMatching
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

    def asBigInt       : BigInt = asTyped[BigInt]
    def asBigDec       : BigDec = asTyped[BigDec]

    def asLocalDate     : LocalDate      = asTyped[LocalDate]
    def asLocalTime     : LocalTime      = asTyped[LocalTime]
    def asLocalDateTime : LocalDateTime  = asTyped[LocalDateTime]
    def asOffsetDateTime: OffsetDateTime = asTyped[OffsetDateTime]
    def asZonedDateTime : ZonedDateTime  = asTyped[ZonedDateTime]
    def asInstant       : Instant        = asTyped[Instant]
    
    def asBinary        : ByteBuffer     = asTyped[ByteBuffer]

    def asEnum         : EnumValue       = asTyped[EnumValue]

    // ---------------------------------------------------------------------------
    def asObj          : Obj     = asTyped[Obj] }

  // ===========================================================================
  object ValueWrapper {
    implicit def to(x: AnyValue): ValueWrapper = new ValueWrapper(x) }

// ===========================================================================
