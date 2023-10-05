package gallia
package whatever

import aptus._
import vldt._Error.Runtime.WhateverOperationForbidden

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
  // - t220318104258 - consider using Dynamic, at least for methods that don't change type?

  // ---------------------------------------------------------------------------
  override def toString: String = formatDefault
    def formatDefault: String = Whatever.formatDefault(any)

  // ===========================================================================
  //redefinition of equals method. See SIP-15, criterion 4. is not allowed in value class
  //  override def equals(that: Any): Boolean = any == that
  //  override def hashCode: Int = any.hashCode()

  // ===========================================================================
  // TODO: t210112165159 - consider === and !== so as to not clash with expected semantics?

  def == (that: String) : TypedWhatever[Boolean] = any.string  == that
  def == (that: Boolean): TypedWhatever[Boolean] = any.boolean == that
  def == (that: Int)    : TypedWhatever[Boolean] = any.int     == that
  def == (that: Long)   : TypedWhatever[Boolean] = any.long    == that
  def == (that: Double) : TypedWhatever[Boolean] = any.double  == that
  // note: no support for byte/short/float (see 220318114510 whatever.md@docs)
  
    // ---------------------------------------------------------------------------
    def == (that: BigInt): TypedWhatever[Boolean] = any.bigInt == that
    def == (that: BigDec): TypedWhatever[Boolean] = any.bigDec == that

    // ---------------------------------------------------------------------------
    def == (that: LocalDate)     : TypedWhatever[Boolean] = any.localDate      == that
    def == (that: LocalTime)     : TypedWhatever[Boolean] = any.localTime      == that
    def == (that: LocalDateTime) : TypedWhatever[Boolean] = any.localDateTime  == that
    def == (that: OffsetDateTime): TypedWhatever[Boolean] = any.offsetDateTime == that
    def == (that: ZonedDateTime) : TypedWhatever[Boolean] = any.zonedDateTime  == that
    def == (that: Instant)       : TypedWhatever[Boolean] = any.instant        == that
    
    // ---------------------------------------------------------------------------    
    def == (that: ByteBuffer)    : TypedWhatever[Boolean] = any.binary         == that

    // ---------------------------------------------------------------------------
    def == (that: EnumValue)(implicit di: DI): TypedWhatever[Boolean] = any.enm            == that

  // ===========================================================================
  def != (that: String) : TypedWhatever[Boolean] = any.string  != that
  def != (that: Boolean): TypedWhatever[Boolean] = any.boolean != that
  def != (that: Int)    : TypedWhatever[Boolean] = any.int     != that
  def != (that: Long)   : TypedWhatever[Boolean] = any.long    != that
  def != (that: Double) : TypedWhatever[Boolean] = any.double  != that
  // note: no support for byte/short/float (see 220318114510 whatever.md@docs)
  
    // ---------------------------------------------------------------------------
    def != (that: BigInt): TypedWhatever[Boolean] = any.bigInt != that
    def != (that: BigDec): TypedWhatever[Boolean] = any.bigDec != that

    // ---------------------------------------------------------------------------
    def != (that: LocalDate)     : TypedWhatever[Boolean] = any.localDate      != that
    def != (that: LocalTime)     : TypedWhatever[Boolean] = any.localTime      != that
    def != (that: LocalDateTime) : TypedWhatever[Boolean] = any.localDateTime  != that
    def != (that: OffsetDateTime): TypedWhatever[Boolean] = any.offsetDateTime != that
    def != (that: ZonedDateTime) : TypedWhatever[Boolean] = any.zonedDateTime  != that
    def != (that: Instant)       : TypedWhatever[Boolean] = any.instant        != that

    // ---------------------------------------------------------------------------
    def != (that: ByteBuffer)    : TypedWhatever[Boolean] = any.binary         != that

    // ---------------------------------------------------------------------------
    def != (that: EnumValue)(implicit di: DI): TypedWhatever[Boolean] = any.enm            != that

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

  // ---------------------------------------------------------------------------
  def - (that: Any): Whatever = 
    that match {
      case x: Whatever => minus(this.any, x.any)
      case _           => minus(this.any, that) }

  // ---------------------------------------------------------------------------
  def / (that: Any): Whatever = 
    that match {
      case x: Whatever => dividedBy(this.any, x.any)
      case _           => dividedBy(this.any, that) }
  
  // ---------------------------------------------------------------------------
  def % (that: Any): Whatever = 
    that match {
      case x: Whatever => modulo(this.any, x.any)
      case _           => modulo(this.any, that) }
  
  // ===========================================================================
  //TODO: t210111135617 - add more common operations?
  //TODO: t210113124437 - cardinality (will need to adapt mechanism to obtain 0 when missing)

  // ---------------------------------------------------------------------------
  @IntSize
  def  sizeString   : TypedWhatever[Int]     = _string(_.size)

  def sizeList      : TypedWhatever[Int]     = _seq(_.size)

  def  isEmptyString: TypedWhatever[Boolean] = _string(_.isEmpty)
  def nonEmptyString: TypedWhatever[Boolean] = _string(_.nonEmpty)

  def startsWith(prefix: String): TypedWhatever[Boolean] = _string(_.startsWith(prefix))
  def endsWith  (suffix: String): TypedWhatever[Boolean] = _string(_.endsWith  (suffix))

  def  toLowerCase: TypedWhatever[String] = _string(_.toLowerCase)
  def  toUpperCase: TypedWhatever[String] = _string(_.toUpperCase)

  def  capitalize: TypedWhatever[String] = _string(x => new collection.StringOps(x).capitalize) // causes issues with scala 3 without the explicit StringOps (not sure why)

  //TODO: replace, remove, stripPrefix, ...

  // ---------------------------------------------------------------------------
  def flip    : TypedWhatever[Boolean] = _boolean(!_)
  def unary_! : TypedWhatever[Boolean] = _boolean(!_)
  
  // ---------------------------------------------------------------------------
  def increment: TypedWhatever[Int] = _int(_ + 1)
  def decrement: TypedWhatever[Int] = _int(_ - 1)

  // ---------------------------------------------------------------------------
  def square     : TypedWhatever[Double] = _number(math.pow(_, 2))
  def sqrt       : TypedWhatever[Double] = _number(math.sqrt(_))
  
  def ln         : TypedWhatever[Double] = _number(math.log(_))
  def log2       : TypedWhatever[Double] = _number(x => math.log(x) / math.log(2))

  def pow(n: Int): TypedWhatever[Double] = _number(math.pow(_, n))

  // ---------------------------------------------------------------------------
  def stringValue: TypedWhatever[String] = _enm(_.stringValue)

  // ===========================================================================
private def _seq[T](f: Seq[_] => T): TypedWhatever[T] = any match {
        case x: Seq[_] => f(x)
        case x         => dataError(matchError("220725141231", x)) }

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

    // ---------------------------------------------------------------------------
    private def _enm[T](f: EnumValue => T): TypedWhatever[T] = any match {
      case x: EnumValue => f(x)
      case x            => dataError(matchError("210113123521", x)) }
}

// ===========================================================================
object Whatever {
  private[whatever] implicit def to   (value: Any):      Whatever    = new      Whatever   (value)
  private           implicit def to[T](value: T)  : TypedWhatever[T] = new TypedWhatever[T](value)

  // ---------------------------------------------------------------------------
  private[gallia] def formatDefault(value: Any): String = WhateverUtils.formatDefault(value)
  private[gallia] def size         (value: Any): Int    = WhateverUtils.size(value)

  // ---------------------------------------------------------------------------
  private[gallia] def whateverOpt(value: Any): Option[Whatever] =          
      value match {
        case None    => None
        case Some(x) => Some(new Whatever(x))
        case      x  => Some(new Whatever(x)) }

    // ---------------------------------------------------------------------------  
    private[gallia] def whatever2Opt(x: Any, y: Any): Option[(Whatever, Whatever)] =
      (whateverOpt(x), whateverOpt(y))
        .toOptionalTuple
  
    // ---------------------------------------------------------------------------
    private[gallia] def whatever3Opt(x: Any, y: Any, z: Any): Option[(Whatever, Whatever, Whatever)] =
      (whateverOpt(x), whateverOpt(y), whateverOpt(z))
        .toSeq
        .toOptionalSeq
        .map(_.force.tuple3)

  // ===========================================================================
  import WhateverImplicits._
    implicit def _toBoolean   (value: Whatever): Boolean    = value.any.boolean
    implicit def _toInt       (value: Whatever): Int        = value.any.int    
    implicit def _toLong      (value: Whatever): Long       = value.any.long 
    implicit def _toDouble    (value: Whatever): Double     = value.any.double 
    implicit def _toString    (value: Whatever): String     = value.any.string
    // note: no support for byte/short/float (see 220318114510 whatever.md@docs)  

      // ---------------------------------------------------------------------------   
      implicit def _toBigInt    (value: Whatever): BigInt = value.any.bigInt 
      implicit def _toBigDec    (value: Whatever): BigDec = value.any.bigDec
      
      // ---------------------------------------------------------------------------
      implicit def _toLocalDate     (value: Whatever): LocalDate      = value.any.localDate
      implicit def _toLocalTime     (value: Whatever): LocalTime      = value.any.localTime
      implicit def _toLocalDateTime (value: Whatever): LocalDateTime  = value.any.localDateTime
      implicit def _toOffsetDateTime(value: Whatever): OffsetDateTime = value.any.offsetDateTime
      implicit def _toZonedDateTime (value: Whatever): ZonedDateTime  = value.any.zonedDateTime
      implicit def _toInstant       (value: Whatever): Instant        = value.any.instant

      // ---------------------------------------------------------------------------
      implicit def _toBinary        (value: Whatever): ByteBuffer        = value.any.binary

      // ---------------------------------------------------------------------------
      implicit def _toEnm           (value: Whatever): EnumValue        = value.any.enm

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
          case _         => boilerplate.WhateverTimes(first, second) } }
  
  // ---------------------------------------------------------------------------
  private[gallia] def plus(first: Any, second: Any): Any =
    first match {
      case x: String => x + second.toString
      case _         => 
        second match {
          case y: String => WhateverOperationForbidden("?+string").throwDataError()
          case _         => boilerplate.WhateverPlus(first, second) } }

  // ---------------------------------------------------------------------------
  private[gallia] def minus(first: Any, second: Any): Any =
    first match {
      case x: String => WhateverOperationForbidden("string-?").throwDataError()
      case _         => 
        second match {
          case y: String => WhateverOperationForbidden("?-string").throwDataError()
          case _         => boilerplate.WhateverMinus(first, second) } }

  // ---------------------------------------------------------------------------
  private[gallia] def dividedBy(first: Any, second: Any): Any =
    first match {
      case x: String => WhateverOperationForbidden("string/?").throwDataError()
      case _         => 
        second match {
          case y: String => WhateverOperationForbidden("?/string").throwDataError()
          case _         => boilerplate.WhateverDividedBy(first, second) } } // comes with all the warts of java's  division

  // ---------------------------------------------------------------------------
  private[gallia] def modulo(first: Any, second: Any): Any =
    first match {
      case x: String => WhateverOperationForbidden("string/?").throwDataError()
      case _         => 
        second match {
          case y: String => WhateverOperationForbidden("?/string").throwDataError()
          case _         => boilerplate.WhateverModulo(first, second) } } // comes with all the warts of java's  division
  
}

// ===========================================================================
