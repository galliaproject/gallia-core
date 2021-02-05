package aptus.misc

// ===========================================================================
object Java {

  @inline def toScala: PartialFunction[Any /* java */, Any /* scala */] = toScalaNonNumber.orElse(toScalaNumber)

    // ---------------------------------------------------------------------------
    @inline def toScalaNonNumber: PartialFunction[Any /* java */, Any /* scala */] = {
      case x: java.lang.Boolean => Boolean.unbox(x)
      case x: String            => x }

    // ---------------------------------------------------------------------------
    @inline def toScalaNumber: PartialFunction[Any /* java */, Any /* scala */] = { // 201102113329
      case x: java.lang.Double  => x.toDouble
      case x: java.lang.Integer => x.toInt

      case x: java.lang.Byte    => x.toByte
      case x: java.lang.Short   => x.toShort
      case x: java.lang.Long    => x.toLong

      case x: java.lang.Float   => x.toFloat

      // the type ascription is for documentation purposes
      case x: java.math.BigDecimal => (x: scala.math.BigDecimal)
      case x: java.math.BigInteger => (x: scala.math.BigInt    ) }

}

// ===========================================================================
