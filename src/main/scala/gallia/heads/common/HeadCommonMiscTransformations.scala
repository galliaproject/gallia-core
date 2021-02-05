package gallia.heads.common

import aptus.String_
import aptus.Double_

import gallia._
import gallia.actions.ActionsUUReducer0._

// ===========================================================================
trait HeadCommonMiscTransformations[F <: HeadCommon[F]] { _: HeadCommon[F] =>

  def transformString (k: RPathW) = transform(_.stringx (k.value))
  def transformInt    (k: RPathW) = transform(_.intx    (k.value))
  def transformDouble (k: RPathW) = transform(_.doublex (k.value))
  def transformBoolean(k: RPathW) = transform(_.booleanx(k.value))
  // exclude: byte, short, ...

  def transformDate    (k: RPathW) = transform(_.datex    (k.value))
  def transformDateTime(k: RPathW) = transform(_.dateTimex(k.value))

  def transformBigInt    (k: RPathW) = transform(_.bigIntx    (k.value))
  def transformBigDecimal(k: RPathW) = transform(_.bigDecimalx(k.value))

  def transformEnum[E <: enumeratum.EnumEntry : WTT](k: RPathW) = transform(_.enumx[E](k.value)) // TODO: try

  // ---------------------------------------------------------------------------
  // TODO: t210110094731
  //def transformString(f: SEL.Transform.Selector): Self2 = ???//transform(_.stringx(f))

  // ---------------------------------------------------------------------------
  def transformObject (k: RPathW) = transform(_.obj (k.value)) // TODO: rename to convey 1 (as oppose to x)
  def transformObjects(k: RPathW) = transform(_.objz(k.value))

    // ---------------------------------------------------------------------------
    def transformGroupObjectsUsing         (f: HeadZ => HeadZ)                    : Self2 = transform(_.objz(_group))   .using(f)
    def transformGroupObjectsUsing         (f: HeadZ => HeadU)    (implicit d: DI): Self2 = transform(_.objz(_group))   .using(f)
    def transformGroupObjectsUsing[D1: WTT](f: HeadZ => HeadV[D1])(implicit d: DI): Self2 = transform(_.objz(_group))(d).using(f)

    def transformLeftObjectsUsing         (f: HeadZ => HeadZ)                    : Self2 = transform(_.objz(_left))   .using(f)
    def transformLeftObjectsUsing         (f: HeadZ => HeadU)    (implicit d: DI): Self2 = transform(_.objz(_left))   .using(f)
    def transformLeftObjectsUsing[D1: WTT](f: HeadZ => HeadV[D1])(implicit d: DI): Self2 = transform(_.objz(_left))(d).using(f)

    def transformRightObjectsUsing         (f: HeadZ => HeadZ)                    : Self2 = transform(_.objz(_right))   .using(f)
    def transformRightObjectsUsing         (f: HeadZ => HeadU)    (implicit d: DI): Self2 = transform(_.objz(_right))   .using(f)
    def transformRightObjectsUsing[D1: WTT](f: HeadZ => HeadV[D1])(implicit d: DI): Self2 = transform(_.objz(_right))(d).using(f)

  // ===========================================================================
  // array ops

  // TODO: use reducer rather:
    @deprecated def toSize (key: RPathW): Self2 = self2 :+ ToSize (TargetQueryUtils.tqqpathz(key)) // or replace with eg transform(_.strings_('f)).using(_.map(_.size).getOrElse(0))
    @deprecated def toSum  (key: RPathW): Self2 = self2 :+ ToSum  (TargetQueryUtils.tqqpathz(key))

    @deprecated def toMean (key: RPathW): Self2 = self2 :+ ToMean (TargetQueryUtils.tqqpathz(key)) // TODO: build in max decimal
    @deprecated def toStdev(key: RPathW): Self2 = self2 :+ ToStdev(TargetQueryUtils.tqqpathz(key)) // TODO: build in max decimal

  // ===========================================================================
  // string ops

    // TODO: def concatenate(x: KeyW, y: KeyW) = new def { asNewKey(nk: KeyW) = fuse(x, y).as(nk).using(_ + _) } - validate strings or seq of same types only?

    def unquoteValueIfApplicable(x: RPathW): Self2 = transform(_.stringx(x)).using(_.unquoteIfApplicable)
    def toUpperCase             (x: RPathW): Self2 = transform(_.stringx(x)).using(_.toUpperCase)
    def toLowerCase             (x: RPathW): Self2 = transform(_.stringx(x)).using(_.toLowerCase)
    def reverseString           (x: RPathW): Self2 = transform(_.stringx(x)).using(_.reverse)
    //  TODO: t210205122644 - see String and StringOps, ...

    // ---------------------------------------------------------------------------
    def reverseString(f: SEL.Transform.Selector): Self2 = ??? // TODO: t210110094731

  // ===========================================================================
  // numerical ops

    @NumberAbstraction
    def square(key: RPathW): Self2 = transform(_.doublex(key)).using(Math.pow(_, 2))
    def sqrt  (key: RPathW): Self2 = transform(_.doublex(key)).using(Math.sqrt(_))
    def log   (key: RPathW): Self2 = transform(_.doublex(key)).using(Math.log (_))
    // ... t210205122644 - provide common ones; multiply, sum, divide, subtract, ...

    def maxDecimals(key : RPathW, n: Int): Self2 = transform(_.doublex(key)).using(_.maxDecimals(n)) //TODO: meta check n > 0

  // ===========================================================================
  // int ops

    def increment(x : RPathW): Self2 = transform(_.intx(x)).using(_ + 1)
    def decrement(x : RPathW): Self2 = transform(_.intx(x)).using(_ - 1)
    //TODO: more, eg: t210205122644 - round, ...

  // ===========================================================================
  // boolean ops

  def flip(x : RPathW): Self2 = transform(_.booleanx(x)).using(!_)

  // ===========================================================================
  // time ops

  def addDay(x : RPathW): Self2 = ???// t210202124121 - need a way to abstract date/dateTime, both can have a day added to
  // TODO: t210205122644 - more time-related...

  // ===========================================================================
  // TODO: t210205122644
  /*
    def addId(value: AnyValue): Self = add(_id, value)
    def addTo(key: RKey) = new {
      def item [T: WTT](value: T)      : Self = ???
      def items[T: WTT](values: Seq[T]): Self = ???

      def items[T: WTT](value1: T, value2: T, more: T*): Self = ??? }

    // ---------------------------------------------------------------------------
    def transformGroup(f: Objs => AnyValue): Self = transform(_group).usingz(f)
    def transformLeft (f: Objs => AnyValue): Self = transform(_left ).usingz(f)
    def transformRight(f: Objs => AnyValue): Self = transform(_right).usingz(f)

    // ---------------------------------------------------------------------------
    def reformatTime(key: RKey, more: RKey*) = new {
      def from(fromPattern: format.DateTimeFormatter) = new _ConvertDate((key +: more), fromPattern)
      def from(fromPattern: String                  ) = new _ConvertDate((key +: more), format.DateTimeFormatter.ofPattern(fromPattern))

      // ---------------------------------------------------------------------------
      class _ConvertDate(keys: RKeys, fromPattern: format.DateTimeFormatter) extends Serializable { //TODO: cache patterns
        def to(toPattern: String                  ): Self = this.to(format.DateTimeFormatter.ofPattern(toPattern))
        def to(toPattern: format.DateTimeFormatter): Self = ??? } }
  */
}

// ===========================================================================
