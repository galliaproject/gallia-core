package gallia
package heads
package common

import aptus.{String_, Double_}

import actions.common.ActionsCommonReducer0._
import actions.common.ActionsCommonTransforms.{TransformObjectCustom, TransformObjectsCustom}

// ===========================================================================
trait HeadCommonMiscTransformations[F <: HeadCommon[F]] { ignored: HeadCommon[F] => // TODO: t210110094731 - add selections throughout

  def transformSole        (f: WV =>  WV)                   : Self2 = new _TransformWhatever(_.soleKey).using(f)
  def transformSole[D: WTT](f: WV => TWV[D])                : Self2 = new _TransformWhatever(_.soleKey).using(f)
  def transformSole[D: WTT](f: WV =>     D)(implicit di: DI): Self2 = new _TransformWhatever(_.soleKey).using(f)

  // ---------------------------------------------------------------------------
  // TODO: t220317154117 - consider a similar transformAll (if all of the same type?) 
  
  // ---------------------------------------------------------------------------
  @PartialTypeMatching
    def transformString (k: RPathW) = transform(_.stringx (k.value))
    def transformInt    (k: RPathW) = transform(_.intx    (k.value))
    def transformDouble (k: RPathW) = transform(_.doublex (k.value))
    def transformBoolean(k: RPathW) = transform(_.booleanx(k.value))
   // excluding less common ones: byte, short, ... (be explicit for those)

  // ---------------------------------------------------------------------------
  // TODO: t210110094731
  //def transformString(f: SEL.Transform.Selector): Self2 = ???//transform(_.stringx(f))
  
  // ---------------------------------------------------------------------------
  // time: 
  def transformStringToLocalDateTime(k: RPathW) = transformString(k).using(_.parseLocalDateTime /* from aptus */)
  def transformStringToLocalDate    (k: RPathW) = transformString(k).using(_.parseLocalDate     /* from aptus */)
  def transformStringToLocalTime    (k: RPathW) = transformString(k).using(_.parseLocalTime     /* from aptus */)

  // ---------------------------------------------------------------------------
  // enum:
  def transformEnumStringValue(k: RPathW) = new _TransformEnumStringValue(k)
   final class _TransformEnumStringValue private[heads] (k: RPathW) {
    def using(f: EnumStringValue => EnumStringValue) =
      transform(_.enm(k)).using {
        _.stringValue.pipe(f).pipe(EnumValue.apply) } }

  // ===========================================================================
  @deprecated def transformObject        (k: RPathW): _TransformU = transform(_.entity  (k.value)) // TODO: rename to convey 1 (as oppose to x)
              def transformEntity        (k: RPathW): _TransformU = transform(_.entity  (k.value)) // TODO: rename to convey 1 (as oppose to x)
  @deprecated def transformAllObjects    (k: RPathW): _TransformZ = transform(_.entities(k.value))
              def transformAllEntities   (k: RPathW): _TransformZ = transform(_.entities(k.value))
  @deprecated def transformAllObjectsTmp            : _TransformZ = transform(_.entities(_tmp))
              def transformAllEntitiesTmp           : _TransformZ = transform(_.entities(_tmp))

  @deprecated("use more explicit transformAllEntities") def transformObjects(k: RPathW) = transformAllEntities(k)

  // ---------------------------------------------------------------------------
  @deprecated def transformSomeObjects (k: RPathW): _TransformSomeObjects = new _TransformSomeObjects(k)
              def transformSomeEntities(k: RPathW): _TransformSomeObjects = new _TransformSomeObjects(k)

    // ---------------------------------------------------------------------------
    @deprecated def transformGroupObjectsUsing          (f: HeadZ => HeadZ)                    : Self2 = transform(_.entities(_group))   .using(f)
                def transformGroupEntitiesUsing         (f: HeadZ => HeadZ)                    : Self2 = transform(_.entities(_group))   .using(f)
    @deprecated def transformGroupObjectsUsing          (f: HeadZ => HeadU)    (implicit d: DI): Self2 = transform(_.entities(_group))   .using(f)
                def transformGroupEntitiesUsing         (f: HeadZ => HeadU)    (implicit d: DI): Self2 = transform(_.entities(_group))   .using(f)
    @deprecated def transformGroupObjectsUsing [D1: WTT](f: HeadZ => HeadV[D1])(implicit d: DI): Self2 = transform(_.entities(_group))(d).using(f)
                def transformGroupEntitiesUsing[D1: WTT](f: HeadZ => HeadV[D1])(implicit d: DI): Self2 = transform(_.entities(_group))(d).using(f)

    @deprecated def transformLeftObjectsUsing          (f: HeadZ => HeadZ)                    : Self2 = transform(_.entities(_left))   .using(f)
                def transformLeftEntitiesUsing         (f: HeadZ => HeadZ)                    : Self2 = transform(_.entities(_left))   .using(f)
    @deprecated def transformLeftObjectsUsing          (f: HeadZ => HeadU)    (implicit d: DI): Self2 = transform(_.entities(_left))   .using(f)
                def transformLeftEntitiesUsing         (f: HeadZ => HeadU)    (implicit d: DI): Self2 = transform(_.entities(_left))   .using(f)
    @deprecated def transformLeftObjectsUsing [D1: WTT](f: HeadZ => HeadV[D1])(implicit d: DI): Self2 = transform(_.entities(_left))(d).using(f)
                def transformLeftEntitiesUsing[D1: WTT](f: HeadZ => HeadV[D1])(implicit d: DI): Self2 = transform(_.entities(_left))(d).using(f)

    @deprecated def transformRightObjectsUsing          (f: HeadZ => HeadZ)                    : Self2 = transform(_.entities(_right))   .using(f)
                def transformRightEntitiesUsing         (f: HeadZ => HeadZ)                    : Self2 = transform(_.entities(_right))   .using(f)
    @deprecated def transformRightObjectsUsing          (f: HeadZ => HeadU)    (implicit d: DI): Self2 = transform(_.entities(_right))   .using(f)
                def transformRightEntitiesUsing         (f: HeadZ => HeadU)    (implicit d: DI): Self2 = transform(_.entities(_right))   .using(f)
    @deprecated def transformRightObjectsUsing [D1: WTT](f: HeadZ => HeadV[D1])(implicit d: DI): Self2 = transform(_.entities(_right))(d).using(f)
                def transformRightEntitiesUsing[D1: WTT](f: HeadZ => HeadV[D1])(implicit d: DI): Self2 = transform(_.entities(_right))(d).using(f)

  // ===========================================================================
  // to/from JSON

  // TODO: an "opaque object" version for each (see t210110094829)
  def parseJsonObjectString(k: RPathW) = new _ParseJsonObjectString(k)
     final class _ParseJsonObjectString private[heads] (k: RPathW) {
      def usingSchema(c: Cls) = transformString(k).toObjUsing(c)(
        atoms.AtomsIX._JsonObjectString.toObj(c)) }
  
    // ---------------------------------------------------------------------------
    def parseJsonArrayString(k: RPathW) = new _ParseJsonArrayString(k)
     final class _ParseJsonArrayString private[heads] (k: RPathW) {
      def usingSchema(c: Cls) = transformString(k).toObjsUsing(c)(
        atoms.AtomsIX._JsonArrayString.toObjs(c)) }
  
  // ---------------------------------------------------------------------------
  /** uses compact form */ def formatJsonObjectString(k: RPathW): Self2 = transformEntityCustom  (k)(_.formatCompactJson)
  /** uses compact form */ def formatJsonArrayString (k: RPathW): Self2 = transformEntitiesCustom(k)(_.formatCompactJson)

    // ---------------------------------------------------------------------------
    @deprecated def transformObjectCustom  [D: WTT](k: RPathW)(f: Obj  => D): Self2 = self2 :+ TransformObjectCustom (TargetQueryUtils.tqrpathz(k), typeNode[D], f)
                def transformEntityCustom  [D: WTT](k: RPathW)(f: Obj  => D): Self2 = self2 :+ TransformObjectCustom (TargetQueryUtils.tqrpathz(k), typeNode[D], f)
    @deprecated def transformObjectsCustom [D: WTT](k: RPathW)(f: Objs => D): Self2 = self2 :+ TransformObjectsCustom(TargetQueryUtils.tqrpathz(k), typeNode[D], f)
                def transformEntitiesCustom[D: WTT](k: RPathW)(f: Objs => D): Self2 = self2 :+ TransformObjectsCustom(TargetQueryUtils.tqrpathz(k), typeNode[D], f)

  // ---------------------------------------------------------------------------    
  @deprecated def zoomOnObject(target: KPathW): Self2 = retain(target.value).unnestAllFrom(target)
              def zoomOnEntity(target: KPathW): Self2 = retain(target.value).unnestAllFrom(target)

  // ===========================================================================
  // array ops

  // TODO: use reducer rather:
    @deprecated def toSize (key: RPathW): Self2 = self2 :+ ToSize (TargetQueryUtils.tqrpathz(key)) // or replace with eg transform(_.strings_('f)).using(_.map(_.size).getOrElse(0))
    @deprecated def toSum  (key: RPathW): Self2 = self2 :+ ToSum  (TargetQueryUtils.tqrpathz(key))

    @deprecated def toMean (key: RPathW): Self2 = self2 :+ ToMean (TargetQueryUtils.tqrpathz(key)) // TODO: build in max decimal
    @deprecated def toStdev(key: RPathW): Self2 = self2 :+ ToStdev(TargetQueryUtils.tqrpathz(key)) // TODO: build in max decimal

  // ===========================================================================
  // string ops

    // TODO: def concatenate(x: KeyW, y: KeyW) = new def { asNewKey(nk: KeyW) = fuse(x, y).as(nk).using(_ + _) } - validate strings or seq of same types only?

    def unquote      (x: RPathW): Self2 = transform(_.stringx(x)).using(_.unquote)
    def toUpperCase  (x: RPathW): Self2 = transform(_.stringx(x)).using(_.toUpperCase)
    def toLowerCase  (x: RPathW): Self2 = transform(_.stringx(x)).using(_.toLowerCase)
    def reverseString(x: RPathW): Self2 = transform(_.stringx(x)).using(_.reverse)
    //  TODO: t210205122644 - see String and StringOps, ...

    // ---------------------------------------------------------------------------
    def reverseString(f: SEL.Transform.Selector): Self2 = ??? // TODO: t210110094731

  // ===========================================================================
  // numerical ops

    @NumberAbstraction
    def square(key: RPathW): Self2 = transform(_.doublex(key)).using(math.pow(_, 2))
    def sqrt  (key: RPathW): Self2 = transform(_.doublex(key)).using(math.sqrt(_))
    def log   (key: RPathW): Self2 = transform(_.doublex(key)).using(math.log (_))
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
