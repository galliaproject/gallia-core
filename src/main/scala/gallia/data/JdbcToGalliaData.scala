package gallia
package data

import aptus.Anything_

// ===========================================================================
object JdbcToGalliaData {

  def convertRecursively(schema: Cls)(o: Obj): Obj =
      schema
        .fields
        .flatMap { field =>
          schema.unknownKeys(o).assert(_.isEmpty) // necessary for union types (see 220615165554)

          o .attemptKey(field.key)
            .map { value =>
              field.key ->
                // TODO: t220615111248 - offer some array/nesting support?
                _basicValue(field.forceBasicType)(value) } }
      .pipe(gallia.obj)

  // ===========================================================================
  private def _basicValue(basicType: BasicType)(value: AnyValue): AnyValue =
    basicType match {
      case BasicType._String  => value.asInstanceOf[String] // casting to ensure values are typed as expected (should be)
      case BasicType._Boolean => value.asInstanceOf[Boolean]
      case BasicType._Double  => value.asInstanceOf[Double]
      case BasicType._Int     => value.asInstanceOf[Int]

      // ---------------------------------------------------------------------------
      case BasicType._Long    => value.asInstanceOf[Long]
      case BasicType._Float   => value.asInstanceOf[Float]

      // ---------------------------------------------------------------------------
      case BasicType._BigInt  => value.asInstanceOf[java.math.BigInteger].pipe(BigInt    .apply)
      case BasicType._BigDec  => value.asInstanceOf[java.math.BigDecimal].pipe(BigDecimal.apply)

      // ---------------------------------------------------------------------------
      case BasicType._LocalDate     => value.asInstanceOf[java.sql.Date]     .toLocalDate     // see 220615092031 for schema counterpart
      case BasicType._LocalTime     => value.asInstanceOf[java.sql.Time]     .toLocalTime     // see 220615092032 for schema counterpart
      case BasicType._LocalDateTime => value.asInstanceOf[java.sql.Timestamp].toLocalDateTime // see 220615092033 for schema counterpart
        // note: java.sql.Timestamp.toInstant does not seem to offer more precision (TODO: confirm)

      // ---------------------------------------------------------------------------
      case BasicType._Binary => value.asInstanceOf[Array[Byte]].pipe(gallia.byteBuffer)

      // ---------------------------------------------------------------------------
      case _: BasicType._Enm  => value.asInstanceOf[String].pipe(BasicType._Enm.parseString)

      // ---------------------------------------------------------------------------
      case
          BasicType._Byte           |
          BasicType._Short          |
          BasicType._OffsetDateTime |
          BasicType. _ZonedDateTime |
          BasicType. _Instant       =>
        // consider parsing string versions for temporal ones?
        aptus.illegalState(s"can't happen, no JDBC counterpart for ${basicType}" /* TODO: t220615092634: confirm */)
    }

}

// ===========================================================================