package gallia.whatever

import gallia._
import gallia.vldt._Error.Runtime.WhateverOperationForbidden

// ===========================================================================
/**
  this is really intended for the most basic and simple operations...
  it may even be removed in the future, depending on feedback.
  the recommended way is to use types explicitly as much as possible:
    more explicit and less likely to result in unpleasant surprises at runtime.
 */
@NumberAbstraction @TypeMatching
class Whatever(private[gallia] val any: Any) extends AnyVal with Serializable { // keep to a minimum
  import Whatever._
  import WhateverImplicits._
  
  // TODO:
  // - t210124100009 - better name?
  // - t210113132613 - try replacing with TypedWhatever[Any]?
  // - t210112115205 - pattern matching error messages throughout

  // ---------------------------------------------------------------------------
  override def toString: String = formatDefault
    def formatDefault: String = Whatever.formatDefault(any)

  // ===========================================================================
  //redefinition of equals method. See SIP-15, criterion 4. is not allowed in value class
  //  override def equals(that: Any): Boolean = any == that
  //  override def hashCode: Int = any.hashCode()

  // ===========================================================================
  // TODO: t210112165159 - consider === and !== so as to not clash with expected semantics?

  def == (that: Boolean): TypedWhatever[Boolean] = any.boolean == that

  def == (that: String) : TypedWhatever[Boolean] = any.string == that

  def   == (that: Int)   : TypedWhatever[Boolean] = any.int    == that
    def == (that: Byte)  : TypedWhatever[Boolean] = any.byte   == that
    def == (that: Short) : TypedWhatever[Boolean] = any.short  == that
    def == (that: Long)  : TypedWhatever[Boolean] = any.long   == that

    def == (that: Double): TypedWhatever[Boolean] = any.double == that
    def == (that: Float) : TypedWhatever[Boolean] = any.float  == that

    def == (that: BigInt)    : TypedWhatever[Boolean] = any.bigInt     == that
    def == (that: BigDecimal): TypedWhatever[Boolean] = any.bigDecimal == that

    def == (that: LocalDate)    : TypedWhatever[Boolean] = any.date     == that
    def == (that: LocalDateTime): TypedWhatever[Boolean] = any.dateTime == that

    // TODO: enums - t210201095414

  // ---------------------------------------------------------------------------
  def != (that: Boolean): TypedWhatever[Boolean] = any.boolean != that

  def != (that: String) : TypedWhatever[Boolean] = any.string != that

  def   != (that: Int)   : TypedWhatever[Boolean] = any.int    != that
    def != (that: Byte)  : TypedWhatever[Boolean] = any.byte   != that
    def != (that: Short) : TypedWhatever[Boolean] = any.short  != that
    def != (that: Long)  : TypedWhatever[Boolean] = any.long   != that

    def != (that: Double): TypedWhatever[Boolean] = any.double != that
    def != (that: Float) : TypedWhatever[Boolean] = any.float  != that

    def != (that: BigInt)    : TypedWhatever[Boolean] = any.bigInt     != that
    def != (that: BigDecimal): TypedWhatever[Boolean] = any.bigDecimal != that

    def != (that: LocalDate)    : TypedWhatever[Boolean] = any.date     != that
    def != (that: LocalDateTime): TypedWhatever[Boolean] = any.dateTime != that

    // TODO: enums - t210201095414

  // ===========================================================================
  // TODO: t210202160407 - provide it for more types (eg String, dates, ...)?
  def <  (that: Number): TypedWhatever[Boolean] = any.number.doubleValue <  that.doubleValue
  def >  (that: Number): TypedWhatever[Boolean] = any.number.doubleValue >  that.doubleValue

  def <= (that: Number): TypedWhatever[Boolean] = any.number.doubleValue <= that.doubleValue
  def >= (that: Number): TypedWhatever[Boolean] = any.number.doubleValue >= that.doubleValue

  // ===========================================================================
  def + (that: Any): Whatever =
    that match {
    case x: Whatever => plus(this.any, x.any)
    case _           => plus(this.any, that) }

  // ---------------------------------------------------------------------------
  def * (that: Any): Whatever = // "foo" * 3 not allowed for now 
    that match {
    case x: Whatever => times(this.any, x.any)
    case _           => times(this.any, that) }

  // ===========================================================================
  //TODO: t210111135617 - add more common operations?
  //TODO: t210113124437 - cardinality (will need to adapt mechanism to obtain 0 when missing)

  // ---------------------------------------------------------------------------
  @IntSize
  def  sizeString   : TypedWhatever[Int]     = _string(_.size)

  def  isEmptyString: TypedWhatever[Boolean] = _string(_.isEmpty)
  def nonEmptyString: TypedWhatever[Boolean] = _string(_.nonEmpty)

  def startsWith(prefix: String): TypedWhatever[Boolean] = _string(_.startsWith(prefix))
  def endsWith  (suffix: String): TypedWhatever[Boolean] = _string(_.endsWith  (suffix))

  def  toLowerCase: TypedWhatever[String] = _string(_.toLowerCase)
  def  toUpperCase: TypedWhatever[String] = _string(_.toUpperCase)

  //TODO: replace, remove, stripPrefix, ...

  // ---------------------------------------------------------------------------
  def flip: TypedWhatever[Boolean] = _boolean(!_)

  // ---------------------------------------------------------------------------
  def increment: TypedWhatever[Int] = _int(_ + 1)
  def decrement: TypedWhatever[Int] = _int(_ - 1)

  // ---------------------------------------------------------------------------
  def square     : TypedWhatever[Double] = _number(math.pow(_, 2))
  def sqrt       : TypedWhatever[Double] = _number(math.sqrt(_))
  
  def ln         : TypedWhatever[Double] = _number(math.log(_))
  def log2       : TypedWhatever[Double] = _number(x => math.log(x) / math.log(2))

  def pow(n: Int): TypedWhatever[Double] = _number(math.pow(_, n))

  // ===========================================================================  
  private def _string[T](f: String => T): TypedWhatever[T] = any match {
        case x: String => f(x)
        case x         => dataError(matchError("210112163810", x)) }

    // ---------------------------------------------------------------------------
    private def _int[T](f: Int => T): TypedWhatever[T] = any match {
        case x: Int => f(x)
        case x      => dataError(matchError("210113123521", x)) }

    // ---------------------------------------------------------------------------
    private def _boolean[T](f: Boolean => T): TypedWhatever[T] = any match {
        case x: Boolean => f(x)
        case x          => dataError(matchError("210113123521", x)) }

    // ---------------------------------------------------------------------------
    private def _number[T](f: Double => T): TypedWhatever[T] = any match {
        case x: Number => f(x.doubleValue)
        case x         => dataError(matchError("210113123521", x)) }

  // ===========================================================================
  private def applyx[T](f: Any => T): TypedWhatever[T] =
    any match {
      case seq: Seq[_]   => new TypedWhatever[T](Left (seq.map(f)))
      case Some(x) => x match {
        case seq: Seq[_] => new TypedWhatever[T](Left (seq.map(f)))
        case sgl         => new TypedWhatever[T](Right(f(sgl))) }
      case sgl           => new TypedWhatever[T](Right(f(sgl))) }
}

// ===========================================================================
object Whatever {
  private[whatever] implicit def to   (value: Any):      Whatever    = new      Whatever         (value)
  private           implicit def to[T](value: T)  : TypedWhatever[T] = new TypedWhatever[T](Right(value))

  // ---------------------------------------------------------------------------
  private[gallia] def formatDefault(value: Any): String = WhateverUtils.formatDefault(value)
  private[gallia] def size         (value: Any): Int    = WhateverUtils.size(value)

  // ===========================================================================
  import WhateverImplicits._
    implicit def toBoolean   (value: Whatever) = value.any.boolean
    implicit def toInt       (value: Whatever) = value.any.int    
    implicit def toDouble    (value: Whatever) = value.any.double 
    implicit def toString    (value: Whatever) = value.any.string   
    
    implicit def toBigInt    (value: Whatever) = value.any.bigInt 
    implicit def toBigDecimal(value: Whatever) = value.any.bigDecimal
    
    implicit def toDate      (value: Whatever) = value.any.date    
    implicit def toDateTime  (value: Whatever) = value.any.dateTime

  // ===========================================================================
  private def matchError(id: String, value : Any)              = { s"TODO:${id}:${value}:${value.getClass}" }
  private def matchError(id: String, value1: Any, value2: Any) = { s"TODO:${id}:${value1}:${value1.getClass}:${value2}:${value2.getClass}" }

  // ===========================================================================
  private[gallia] def times(first: Any, second: Any): Any =
    first match {
      case x: String => WhateverOperationForbidden("string*?").throwDataError()
      case _         => 
        second match {
          case y: String => WhateverOperationForbidden("?*string").throwDataError()
          case _         => whatever.WhateverTimes(first, second) } }
  
  // ---------------------------------------------------------------------------
  private[gallia] def plus(first: Any, second: Any): Any =
    first match {
      case x: String => x + second.toString
      case _         => 
        second match {
          case y: String => WhateverOperationForbidden("?+string").throwDataError()
          case _         => whatever.WhateverPlus(first, second) } }

}

// ===========================================================================
