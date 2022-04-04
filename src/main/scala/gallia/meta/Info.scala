package gallia
package meta

import aptus.String_

import reflect.Container._
import domain.SortingPair
import vldt.SpecialCardiMode

// ===========================================================================
case class Info(container: Container, containee: Containee) extends InfoLike {
    protected override val _container: Container = container
    protected override val _containee: Containee = containee

    // ===========================================================================
    override def toString = formatDefault

      // ---------------------------------------------------------------------------
      def formatDefault             : String = formatDefault("\t")
      def formatDefault(sep: String): String =
        containee match {
          case basic: BasicType => s"${container}${sep}${basic}"
          case nesting: Cls     => s"${container}\t${nesting.formatDefault.sectionAllOff}" }

    // ===========================================================================
    def compare(pair: SortingPair)(x: AnyValue, y: AnyValue): Int = // for ObjOrdering
      containee match {
        case tipe: BasicType => tipe.compare(container, pair.descending, pair.missingLast)(x, y)
        case r: Cls          => ??? } // TODO

    // ---------------------------------------------------------------------------
    def superPair(pair: SortingPair) = // for sorting
      containee match {
        case tipe: BasicType => tipe.superPair(container, pair.descending, pair.missingLast)
        case r: Cls          => ??? } // TODO

    // ===========================================================================
    def nestingTypeOpt: Option[Cls] = _containee.nestingOpt

    def forceNestedClass: Cls = _containee.nestingOpt.get

    // ===========================================================================
    def transformContainer  (f: Container => Container): Info = copy(container = f(this.container))

    def transformContainee  (f: Containee => Containee): Info = copy(containee = f(this.containee))
    def transformBasicType  (f: BasicType => BasicType): Info = copy(containee = f(this.containee.leafOpt   .get))
    def transformNestedClass(f: Cls       => Cls      ): Info = copy(containee = f(this.containee.nestingOpt.get))

    // ---------------------------------------------------------------------------
    def updateContainer(newValue: Container): Info = transformContainer(_ => newValue)

    def updateContainer(newValue: Container.type => Container): Info = transformContainer(_ => newValue(Container))

    def updateContainee(newValue: BasicType): Info = transformContainee(_ => newValue)
    def updateContainee(newValue: Cls)      : Info = transformContainee(_ => newValue)
    def updateContainee(newValue: Containee): Info = transformContainee(_ => newValue)

    // ---------------------------------------------------------------------------
    def    toRequired: Info = Info(if (isMultiple) _Nes else _One, containee)
    def toNonRequired: Info = Info(if (isMultiple) _Pes else _Opt, containee)

    def    toMultiple: Info = Info(if (isRequired) _Nes else _Pes, containee)
    def toNonMultiple: Info = Info(if (isRequired) _One else _Opt, containee)
    
    // ---------------------------------------------------------------------------
    @PartialTypeMatching
      def toBoolean: Info = Info(container, BasicType._Boolean)
      def toStr    : Info = Info(container, BasicType._String)
      def toInt    : Info = Info(container, BasicType._Int)
      def toDouble : Info = Info(container, BasicType._Double)

    // ---------------------------------------------------------------------------
    def potentiallyProcessNesting(value: AnyValue): AnyValue =
      nestingTypeOpt
       .map { nestedClass =>
         container.containerWrap(nestedClass.valueToObj)(value) }
       .getOrElse(value)
  }

  // ===========================================================================
  object Info {
    def forceFrom[T : WTT]: Info = TypeNode.parse[T].forceNonBObjInfo

    // ---------------------------------------------------------------------------
    def    string  = Info(_One, BasicType._String)

    def oneString  = Info(_One, BasicType._String)
    def oneInt     = Info(_One, BasicType._Int)
    def oneDouble  = Info(_One, BasicType._Double)
    def oneBoolean = Info(_One, BasicType._Boolean)

    def optString  = Info(_Opt, BasicType._String)
    def optInt     = Info(_Opt, BasicType._Int)
    def optDouble  = Info(_Opt, BasicType._Double)
    def optBoolean = Info(_Opt, BasicType._Boolean)

    // ---------------------------------------------------------------------------
    def from(container: Container, tipe: BasicType) = Info(container, tipe)

      def one(tipe: BasicType) = Info(_One, tipe)
      def opt(tipe: BasicType) = Info(_Opt, tipe)
      def pes(tipe: BasicType) = Info(_Pes, tipe)
      def nes(tipe: BasicType) = Info(_Nes, tipe)

    // ---------------------------------------------------------------------------
    def one(tipe: Cls) = Info(_One, tipe)
    def opt(tipe: Cls) = Info(_Opt, tipe)
    def pes(tipe: Cls) = Info(_Pes, tipe)
    def nes(tipe: Cls) = Info(_Nes, tipe)

    // ---------------------------------------------------------------------------
    def optOrOne(optional: Boolean)(tipe: Cls) = if (optional) Info.opt(tipe) else Info.one(tipe)
    def pesOrNes(optional: Boolean)(tipe: Cls) = if (optional) Info.pes(tipe) else Info.nes(tipe)

    // ---------------------------------------------------------------------------
    def from(container: Container, tipe: Cls) = Info(container, tipe)

    def from(optional: Boolean, multiple: Boolean)(tipe: Cls): Info =
      if (multiple)
        if (optional) Info.pes(tipe)
        else          Info.nes(tipe)
      else
        if (optional) Info.opt(tipe)
        else          Info.one(tipe)

    // ---------------------------------------------------------------------------
    def forceNonBObjFrom(node: TypeNode): Info = InfoUtils.forceNonBObjInfo(node)

    // ===========================================================================
    def combine(left: Info, right: Info): Info = {
      require(
        vldt.MetaValidationCompatibility.compatible(left, right, SpecialCardiMode.Normal),
        (left, right))

      Info(
          Container.combine(left.container, right.container),
          Containee.combine(left.containee, right.containee))
    }

  }

// ===========================================================================
