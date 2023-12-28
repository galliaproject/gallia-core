package gallia
package reflect
package lowlevel

import scala.reflect.ClassTag

// ===========================================================================
/** differs based on 2.x vs 3.x */
trait ReflectionTypesAbstraction {
  type CT[T] = ClassTag[T]

  // ===========================================================================
  private[gallia] case class WTT[T](
    typeNode       :        TypeNode,
    ctag           :        ClassTag[T],
    instantiatorOpt: Option[Instantiator])

  // ---------------------------------------------------------------------------
  private[gallia] type CWTT[T] = WTT[T] // to be phased out

  // ===========================================================================
  trait LowPriority {
    inline given [T]: WTT[T] = {
      val (typeNode, instantiator, classTag) = tripletMacro[T]

      WTT[T](
        typeNode,
        classTag,
        if (instantiator.isPlaceholder) None else Some(instantiator)) } }

  // ---------------------------------------------------------------------------
  private inline def tripletMacro[A]: (TypeNode, Instantiator, ClassTag[A]) = ${macros3.PairCreatorMacro3.apply[A]}

  // ===========================================================================
  trait HighPriority extends LowPriority {
    given _string  : WTT[String]  = WttBuiltIns._String .asInstanceOf[WTT[String]]

    given _boolean : WTT[Boolean] = WttBuiltIns._Boolean.asInstanceOf[WTT[Boolean]]
    given _int     : WTT[Int]     = WttBuiltIns._Int    .asInstanceOf[WTT[Int]]
    given _double  : WTT[Double]  = WttBuiltIns._Double .asInstanceOf[WTT[Double]]

    // ---------------------------------------------------------------------------
    given _byte    : WTT[Byte]  = WttBuiltIns._Byte .asInstanceOf[WTT[Byte]]
    given _short   : WTT[Short] = WttBuiltIns._Short.asInstanceOf[WTT[Short]]
    given _song    : WTT[Long]  = WttBuiltIns._Long .asInstanceOf[WTT[Long]]

    given _float   : WTT[Float] = WttBuiltIns._Float.asInstanceOf[WTT[Float]]

    // ---------------------------------------------------------------------------
    given _bigInt  : WTT[BigInt]     = WttBuiltIns._BigInt    .asInstanceOf[WTT[BigInt]]
    given _bigDec  : WTT[BigDecimal] = WttBuiltIns._BigDecimal.asInstanceOf[WTT[BigDecimal]]

    // ---------------------------------------------------------------------------
    given _localDate     : WTT[java.time.LocalDate]      = WttBuiltIns._LocalDate     .asInstanceOf[WTT[java.time.LocalDate]]
    given _localTime     : WTT[java.time.LocalTime]      = WttBuiltIns._LocalTime     .asInstanceOf[WTT[java.time.LocalTime]]
    given _localDateTime : WTT[java.time.LocalDateTime]  = WttBuiltIns._LocalDateTime .asInstanceOf[WTT[java.time.LocalDateTime]]
    given _offsetDateTime: WTT[java.time.OffsetDateTime] = WttBuiltIns._OffsetDateTime.asInstanceOf[WTT[java.time.OffsetDateTime]]
    given _zonedDateTime : WTT[java.time.ZonedDateTime]  = WttBuiltIns._ZonedDateTime .asInstanceOf[WTT[java.time.ZonedDateTime]]
    given _instant       : WTT[java.time.Instant]        = WttBuiltIns._Instant       .asInstanceOf[WTT[java.time.Instant]]

    // ---------------------------------------------------------------------------
    given _byteBuffer  : WTT[java.nio.ByteBuffer] = WttBuiltIns._ByteBuffer.asInstanceOf[WTT[java.nio.ByteBuffer]] }

  // ===========================================================================
  private[gallia] object WTT extends HighPriority
}

// ===========================================================================
