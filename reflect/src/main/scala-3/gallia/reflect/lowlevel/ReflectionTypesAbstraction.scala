package gallia
package reflect
package lowlevel

import scala.reflect.{classTag, ClassTag}

// ===========================================================================
/** differs based on 2.x vs 3.x */
trait ReflectionTypesAbstraction {

  /** stood for WeakTypeTag in scala 2.x, has a life of its own in scala 3.x */
  private[gallia] case class WTT[T](
      typeNode       :        TypeNode,
      ctag           :        ClassTag[T],
      instantiatorOpt: Option[Instantiator]) {
    def instantiator(implicit ev: T <:< enumeratum.EnumEntry): Instantiator =
      instantiatorOpt.get /* guaranteed by design for EnumEntry */ }

  // ===========================================================================
  trait LowPriority {

    // mostly intended to handle data classes
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
    given _long    : WTT[Long]  = WttBuiltIns._Long .asInstanceOf[WTT[Long]]

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
    given _byteBuffer  : WTT[java.nio.ByteBuffer] = WttBuiltIns._ByteBuffer.asInstanceOf[WTT[java.nio.ByteBuffer]]

    // ===========================================================================
    given optInt[T: WTT]: WTT[Option[T]] = {
      val wttt = implicitly[WTT[T]]
      WTT[Option[T]](TypeNodeBuiltIns.scalaOption(wttt.typeNode), classTag[Option[T]],
wttt.instantiatorOpt // TODO: wrap t240124111123
      ).asInstanceOf[WTT[Option[T]]]
    }

    // ---------------------------------------------------------------------------
    given seqInt[T: WTT]: WTT[Seq[T]] = {
      val wttt = implicitly[WTT[T]]
      WTT[Seq[T]](TypeNodeBuiltIns.scalaSeq(wttt.typeNode), classTag[Seq[T]],
wttt.instantiatorOpt // TODO: wrap t240124111123
      ).asInstanceOf[WTT[Seq[T]]]
    }
  }

  // ===========================================================================
  private[gallia] object WTT extends HighPriority

  // ---------------------------------------------------------------------------
  def typeArg       [T: WTT, U]: WTT[U] = implicitly[WTT[T]].typeNode.forceSoleTypeArg                 .pipe { x => WTT[U](x, null /* TODO: 240124111257 */, None) } // eg for Seq[T]
  def typeArgTypeArg[T: WTT, U]: WTT[U] = implicitly[WTT[T]].typeNode.forceSoleTypeArg.forceSoleTypeArg.pipe { x => WTT[U](x, null /* TODO: 240124111257 */, None) } // eg for Seq[Option[T]]
}

// ===========================================================================
