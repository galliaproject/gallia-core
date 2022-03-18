package gallia
package whatever

// ===========================================================================
/** when type is known (eg _.size will always result in a Int) but we need to remain in "whatever" territory */
class TypedWhatever[+T](val typed: T) extends Serializable { // can't be AnyVal since Whatever already is
  import Whatever._
  import WhateverImplicits._

  // ---------------------------------------------------------------------------
  override def toString: String = formatDefault

    def formatDefault: String = WhateverUtils.formatDefault(typed)

  // ---------------------------------------------------------------------------
  private[gallia] def map[T2](f: T => T2): TypedWhatever[T2] = new TypedWhatever[T2](f(typed))

    private[gallia] def mapNumber [T2](f: Double  => T2): TypedWhatever[T2] = map { x => f(x.number.doubleValue) }
    private[gallia] def mapInt    [T2](f: Int     => T2): TypedWhatever[T2] = map { x => f(x.int) }
    private[gallia] def mapBoolean[T2](f: Boolean => T2): TypedWhatever[T2] = map { x => f(x.boolean) }
    private[gallia] def mapString [T2](f: String  => T2): TypedWhatever[T2] = map { x => f(x.string) }
  
  // ===========================================================================
  def unary_!                      (implicit ev: T <:< Boolean): TypedWhatever[Boolean] = mapBoolean { !_ }

  def && (y: TypedWhatever[Boolean])(implicit ev: T <:< Boolean): TypedWhatever[Boolean] = mapBoolean { _ && y.typed.boolean }
  def || (y: TypedWhatever[Boolean])(implicit ev: T <:< Boolean): TypedWhatever[Boolean] = mapBoolean { _ || y.typed.boolean }

  // ---------------------------------------------------------------------------
  def == (that: Whatever): TypedWhatever[Boolean] = map(_.any == that.any)
  def != (that: Whatever): TypedWhatever[Boolean] = map(_.any != that.any)

  // ---------------------------------------------------------------------------
  def <  (that: Whatever): TypedWhatever[Boolean] = map(_.any.number.doubleValue <  that.any.number.doubleValue)
  def >  (that: Whatever): TypedWhatever[Boolean] = map(_.any.number.doubleValue >  that.any.number.doubleValue)
  def <= (that: Whatever): TypedWhatever[Boolean] = map(_.any.number.doubleValue <= that.any.number.doubleValue)
  def >= (that: Whatever): TypedWhatever[Boolean] = map(_.any.number.doubleValue >= that.any.number.doubleValue)

  // ---------------------------------------------------------------------------
  def + (value: Whatever): TypedWhatever[T] = map(x => plus     (x.any, value.any).asInstanceOf[T])
  def * (value: Whatever): TypedWhatever[T] = map(x => times    (x.any, value.any).asInstanceOf[T])
  
  def - (value: Whatever): TypedWhatever[T] = map(x => minus    (x.any, value.any).asInstanceOf[T])
  def / (value: Whatever): TypedWhatever[T] = map(x => dividedBy(x.any, value.any).asInstanceOf[T])
  
  def % (value: Whatever): TypedWhatever[T] = map(x => modulo   (x.any, value.any).asInstanceOf[T])
  
    // ---------------------------------------------------------------------------
    def + (value: TypedWhatever[_ >: Number]): TypedWhatever[T] = this.+(new Whatever(value.typed))
    def * (value: TypedWhatever[_ >: Number]): TypedWhatever[T] = this.*(new Whatever(value.typed))

    def - (value: TypedWhatever[_ >: Number]): TypedWhatever[T] = this.-(new Whatever(value.typed))
    def / (value: TypedWhatever[_ >: Number]): TypedWhatever[T] = this./(new Whatever(value.typed))

    def % (value: TypedWhatever[_ >: Number]): TypedWhatever[T] = this.%(new Whatever(value.typed))

      // ---------------------------------------------------------------------------
      def + (value: Number): TypedWhatever[T] = this.+(new Whatever(value))
      def * (value: Number): TypedWhatever[T] = this.*(new Whatever(value))
  
      def - (value: Number): TypedWhatever[T] = this.-(new Whatever(value))
      def / (value: Number): TypedWhatever[T] = this./(new Whatever(value))
  
      def % (value: Number): TypedWhatever[T] = this.%(new Whatever(value))

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
