package gallia.whatever

import gallia._

// ===========================================================================
/** when type is known (eg _.size will always result in a Int) but we need to remain in "whatever" land */
class TypedWhatever[T](val either: Either[Seq[T], T]) extends AnyVal with Serializable {
  import Whatever._
  import WhateverImplicits._
  
  // ---------------------------------------------------------------------------
  override def toString: String = formatDefault
    def formatDefault: String = WhateverUtils.formatDefault(either.fold(identity, identity))

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
  def sqrt       : TypedWhatever[Double] = mapNumber(math.sqrt(_))
  
  def ln         : TypedWhatever[Double] = mapNumber(math.log(_))
  def log2       : TypedWhatever[Double] = mapNumber(x => math.log(x) / math.log(2))
  
  def pow(n: Int): TypedWhatever[Double] = mapNumber(math.pow(_, n))
}

// ===========================================================================
