package gallia.whatever

import gallia._
import aptus.Seq_
import aptus.Int_
import aptus.Double_

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
  // addition

  def + (that: String): TypedWhatever[String] = applyx(_.toString + that)
  def + (that: Int)   : TypedWhatever[Int]    = applyx(_.int + that) // limitation... see 210112140145 - TODO: workaround
  def + (that: Double): TypedWhatever[Double] = applyx(_.number.doubleValue + that)

  // ---------------------------------------------------------------------------
  def + (that: Whatever): Whatever = plus(this.any, that.any) // eg from fusion

  // ===========================================================================
  // multiplication: "foo" * 3 not allowed for now

//  def * (that: Int)   : TypedWhatever[Int]    = applyx(_.int                * that) // limitation... see 210112140145 - TODO: workaround
//  def * (that: Double): TypedWhatever[Double] = applyx(_.number.doubleValue * that)
//
  // ---------------------------------------------------------------------------
  def * (that: Whatever): Whatever = times(this.any, that.any)  

  // ===========================================================================
  //TODO: t210111135617 - add more common operations?
  //TODO: t210113124437 - cardinality (will need to adapt mechanism to obtain 0 when missing)

  // ---------------------------------------------------------------------------
  @IntSize
  def  sizeString   : TypedWhatever[Int]     = _string("210112163810")(_.size)

  def  isEmptyString: TypedWhatever[Boolean] = _string("210112163810")(_.isEmpty)
  def nonEmptyString: TypedWhatever[Boolean] = _string("210112163810")(_.nonEmpty)

  def startsWith(prefix: String): TypedWhatever[Boolean] = _string("210112163810")(_.startsWith(prefix))
  def endsWith  (suffix: String): TypedWhatever[Boolean] = _string("210112163810")(_.endsWith  (suffix))

  def  toLowerCase: TypedWhatever[String] = _string("210112163810")(_.toLowerCase)
  def  toUpperCase: TypedWhatever[String] = _string("210112163810")(_.toUpperCase)

  //TODO: replace, remove, stripPrefix, ...

  // ---------------------------------------------------------------------------
  def flip: TypedWhatever[Boolean] = _boolean("210113123521")(!_)

  // ---------------------------------------------------------------------------
  def increment: TypedWhatever[Int] = _int("210113123521")(_ + 1)
  def decrement: TypedWhatever[Int] = _int("210113123521")(_ - 1)

  // ---------------------------------------------------------------------------
  def square     : TypedWhatever[Double] = _number("210113123521")(math.pow(_, 2))
  def ln         : TypedWhatever[Double] = _number("210113123521")(math.log(_))
  def pow(n: Int): TypedWhatever[Double] = _number("210113123521")(math.pow(_, n))
  def sqrt       : TypedWhatever[Double] = _number("210113123521")(math.sqrt(_))

  // ===========================================================================
  private def _string[T](id: String)(f: String => T): TypedWhatever[T] = any match {
        case x: String => f(x)
        case x         => dataError(matchError(id, x)) }

    // ---------------------------------------------------------------------------
    private def _int[T](id: String)(f: Int => T): TypedWhatever[T] = any match {
        case x: Int => f(x)
        case x      => dataError(matchError(id, x)) }

    // ---------------------------------------------------------------------------
    private def _boolean[T](id: String)(f: Boolean => T): TypedWhatever[T] = any match {
        case x: Boolean => f(x)
        case x          => dataError(matchError(id, x)) }

    // ---------------------------------------------------------------------------
    private def _number[T](id: String)(f: Double => T): TypedWhatever[T] = any match {
        case x: Number => f(x.doubleValue)
        case x         => dataError(matchError(id, x)) }

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
  implicit def to(x: Any): Whatever = new Whatever(x)

  // ===========================================================================
  @TypeMatching
  implicit class Any_(u: Any) {
    def boolean    = u match { case x: Boolean => x; case _ => dataError(error(u, "210113130850", classOf[Boolean])) }
    def string     = u match { case x: String  => x; case _ => dataError(error(u, "210113130851", classOf[String]))  }

    def number     = u match { case x: Number  => x; case _ => dataError(error(u, "210113130852", classOf[Number])) }

    def int        = u match { case x: Int     => x; case _ => dataError(error(u, "210112140145", classOf[Int]))    }
    def double     = u match { case x: Double  => x; case _ => dataError(error(u, "210113130854", classOf[Double])) }

    def byte       = u match { case x: Byte    => x; case _ => dataError(error(u, "210113130855", classOf[Byte]))  }
    def short      = u match { case x: Short   => x; case _ => dataError(error(u, "210113130856", classOf[Short])) }
    def long       = u match { case x: Long    => x; case _ => dataError(error(u, "210113130857", classOf[Long]))  }
    def float      = u match { case x: Float   => x; case _ => dataError(error(u, "210113130858", classOf[Float])) }

    def bigInt     = u match { case x: BigInt     => x; case _ => dataError(error(u, "210113130859", classOf[BigInt]))  }
    def bigDecimal = u match { case x: BigDecimal => x; case _ => dataError(error(u, "210113130900", classOf[BigDecimal])) }

    def date       = u match { case x: LocalDate     => x; case _ => dataError(error(u, "210113130901", classOf[LocalDate]))  }
    def dateTime   = u match { case x: LocalDateTime => x; case _ => dataError(error(u, "210113130902", classOf[LocalDateTime])) }

    // TODO: enum - t210201095414
  }

  // ===========================================================================
  private def error(value: Any, id: String, klass: Class[_]) = s"TODO:expected ${klass}:${id}:${value}:${value.getClass}"

  // ---------------------------------------------------------------------------
  private def matchError(id: String, value : Any)              = { s"TODO:${id}:${value}:${value.getClass}" }
  private def matchError(id: String, value1: Any, value2: Any) = { s"TODO:${id}:${value1}:${value1.getClass}:${value2}:${value2.getClass}" }

  // ===========================================================================
  private[gallia] def times(first: Any, second: Any): Any = whatever.WhateverTimes(first, second)

  private[gallia] def plus (first: Any, second: Any): Any =
    first match {
      case x: String => x + second.toString
      case _         => whatever.WhateverPlus(first, second) }

  // ===========================================================================
  private[gallia] def size(value: Any): Int =
    value match {
      case x: Whatever => x.any match {
        case None | Nil => 0
        case y: Some[_] => y.size
        case y: Seq [_] => y.size
        case y          => 1 }
      case x: Iterable[_] => x.size // TODO: check this one works
      case x              => 1
    }

  // ===========================================================================
  private[gallia] def formatDefault(any: Any): String =
      any match {

        // legit?
        case None           => "None"
        case Nil            => "Nil"

        case Some(value)    => s"Some(${formatIndividualValue(value)})"
        case values: Seq[_] => s"Seq(${values.map(formatIndividualValue).join(",")})"

        case value          => formatIndividualValue(value) }

      // ---------------------------------------------------------------------------
      private def formatIndividualValue(value: Any) = value match {
          case x: Int    => x.formatExplicit
          case x: Long   => x.formatExplicit
          case x: Double => x.formatExplicit
          //TODO: t210202160709 - more
          case x         => x.toString }
}

// ===========================================================================
/** when type is known (eg +("foo") will always result in a String) but we need to remain in "whatever" land */
class TypedWhatever[T](val either: Either[Seq[T], T]) extends AnyVal with Serializable { import Whatever._

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      def formatDefault: String = Whatever.formatDefault(any = either.fold(identity, identity))

    // ---------------------------------------------------------------------------
    def unary_!                      (implicit ev: T =:= Boolean): TypedWhatever[Boolean] = map { !_.boolean }

    def &&(y: TypedWhatever[Boolean])(implicit ev: T =:= Boolean): TypedWhatever[Boolean] = map { _.boolean && y.boolean }
    def ||(y: TypedWhatever[Boolean])(implicit ev: T =:= Boolean): TypedWhatever[Boolean] = map { _.boolean || y.boolean }
    def ^ (y: TypedWhatever[Boolean])(implicit ev: T =:= Boolean): TypedWhatever[Boolean] = map { _.boolean ^  y.boolean }
    // TODO: also do | and &?

    // ---------------------------------------------------------------------------
    private[gallia] def map[T2](f: T => T2): TypedWhatever[T2] = new TypedWhatever[T2](either match {
        case Left (l) => Left (l.map(f))
        case Right(r) => Right(f(r)) })

      private[gallia] def mapNumber [T2](f: Double  => T2): TypedWhatever[T2] = map { x => f(x.number.doubleValue) }
      private[gallia] def mapInt    [T2](f: Int     => T2): TypedWhatever[T2] = map { x => f(x.int) }
      private[gallia] def mapBoolean[T2](f: Boolean => T2): TypedWhatever[T2] = map { x => f(x.boolean) }
      private[gallia] def mapString [T2](f: String  => T2): TypedWhatever[T2] = map { x => f(x.string) }

    // ---------------------------------------------------------------------------
    private[gallia] def forceOne: T = either match {
      case Left (l) => dataError(s"TODO:210112155242:${l}")
      case Right(r) => r }

    // ---------------------------------------------------------------------------
    private[gallia] def value: Any = either.fold(identity, identity)

    // ===========================================================================
    def == (that: Whatever): TypedWhatever[Boolean] = map(_.any == that.any)
    def != (that: Whatever): TypedWhatever[Boolean] = map(_.any != that.any)

    // ---------------------------------------------------------------------------
    def <  (that: Whatever): TypedWhatever[Boolean] = map(_.any.number.doubleValue <  that.any.number.doubleValue)
    def >  (that: Whatever): TypedWhatever[Boolean] = map(_.any.number.doubleValue >  that.any.number.doubleValue)
    def <= (that: Whatever): TypedWhatever[Boolean] = map(_.any.number.doubleValue <= that.any.number.doubleValue)
    def >= (that: Whatever): TypedWhatever[Boolean] = map(_.any.number.doubleValue >= that.any.number.doubleValue)

    // ---------------------------------------------------------------------------
    def + (value: Whatever): TypedWhatever[T] = map(x => plus (x.any, value.any).asInstanceOf[T])
    def * (value: Whatever): TypedWhatever[T] = map(x => times(x.any, value.any).asInstanceOf[T])

    // ---------------------------------------------------------------------------
    @IntSize
    def  sizeString   : TypedWhatever[Int]     = mapString(_.size)

    def  isEmptyString: TypedWhatever[Boolean] = mapString(_.isEmpty)
    def nonEmptyString: TypedWhatever[Boolean] = mapString(_.nonEmpty)

    def  toLowerCase: TypedWhatever[String] = mapString(_.toLowerCase)
    def  toUpperCase: TypedWhatever[String] = mapString(_.toUpperCase)

    // ---------------------------------------------------------------------------
    def flip: TypedWhatever[Boolean] = mapBoolean(!_)

    // ---------------------------------------------------------------------------
    def increment: TypedWhatever[Int] = mapInt(_ + 1)
    def decrement: TypedWhatever[Int] = mapInt(_ - 1)

    // ---------------------------------------------------------------------------
    def square     : TypedWhatever[Double] = mapNumber(math.pow(_, 2))
    def ln         : TypedWhatever[Double] = mapNumber(math.log(_))
    def pow(n: Int): TypedWhatever[Double] = mapNumber(math.pow(_, n))
    def sqrt       : TypedWhatever[Double] = mapNumber(math.sqrt(_))
  }

  // ===========================================================================
  object TypedWhatever {
    implicit def to[T](value: T): TypedWhatever[T] = new TypedWhatever[T](Right(value))
  }

// ===========================================================================
