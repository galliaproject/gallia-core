package gallia
package heads
package common

import aptus.{Anything_, String_}

import aptus.Separator

import actions.common.ActionsCommonSomewhatBasics._
import actions.common.ActionsCommonConverts._
import actions.common.ActionsCommonDeserialize._
import domain._
import trgt._
import trgt.utils.TypedTargetQueryUtils._

// ===========================================================================
trait HeadCommonSomewhatBasics[F <: HeadCommon[F]] { ignored: HeadCommon[F] =>
  private implicit def _toSelf2(action: ActionUU): Self2 = self2 :+ action // TODO: move up

  // ---------------------------------------------------------------------------
  // remove conditionally
  // note: 210111134300 - may need to name the whatever version differently, else creates  lots of problems
  //   most of the time, we expect the Whatever version to be used though...

  def removeIfValueFor(f : RPathW )                           = new _RemoveConditionallyWhatever(RPathWz.from(f))
  def removeIfValueFor(f : RPathWz)                           = new _RemoveConditionallyWhatever(f)
  def removeIfValueFor(f1: RPathW, f2: RPathW, more: RPathW*) = new _RemoveConditionallyWhatever(f1, f2 ,more) // keep?

  // ---------------------------------------------------------------------------
  def removeIfValueFor[T: WTT](f: RemoveIf[T]) = new _RemoveConditionally[T](TSL.RemoveIf.resolves(f)) // TODO: accept renaming

    // TODO: t210111134021 - forbid is(None)

  // ---------------------------------------------------------------------------
  def removeValueFor(target: KeyW)                         : _RemoveConditionally2 = removeValueFor(_.explicit(target))
  def removeValueFor(selector: SEL.RemoveValueFor.Selector): _RemoveConditionally2 = new _RemoveConditionally2(SEL.RemoveValueFor.resolve(selector))

    // ===========================================================================
    final class _RemoveConditionally[T: WTT] private[common](target: TtqRPathz) {
      def is     (value: Any): Self2 = matches(_ == value)
      def matches(pred: T => Boolean): Self2 =
        RemoveConditionally(target, pwrap(pred)) }

    // ===========================================================================
    final class _RemoveConditionallyWhatever private[common](target: TqRPathz) {
      def isEmptyString    : Self2 = is("")
      def is   (value: Any): Self2 = RemoveConditionallyWhatever(target, value)
      def isNot(value: Any): Self2 = RemoveConditionallyWhatever(target, value) }

    // ===========================================================================
    final class _RemoveConditionally2 private[common] (target: TqKey) {

      def ifValueFor(reference: KeyW) = new _IfValueFor1(reference)
       final class _IfValueFor1 private[heads] (reference: KeyW) {
        def is(value: Any): Self2 =
          RemoveConditionally2Whatever(reference.value, target, value) }

      // ---------------------------------------------------------------------------
      def ifValueFor[T: WTT](reference: IfValueFor[T]) = new _IfValueFor2(reference)
         final class _IfValueFor2[T: WTT] private[heads] (reference: IfValueFor[T]) {
          def is     (value: Any)        : Self2 = matches(_ == value)
          def matches(pred: T => Boolean): Self2 =
            RemoveConditionally2(TSL.IfValueFor.resolve2(reference), target, pwrap(pred)) } }

  // ===========================================================================
  // set default

  // TODO: t201005102404 - for multiple: offer backfill/forward fill
  // TODO: t210129093607 - offer a setDefaults('foo -> 1, 'bar -> "baz", ...)

  def setDefaultFor(path : RPathW ): _SetDefaultFor = setDefaultFor(RPathWz.from(path))
  def setDefaultFor(paths: RPathWz): _SetDefaultFor = new _SetDefaultFor(TargetQueryUtils.tqrpathz(paths.rpathz))

  def setDefaultFor(path1: RPathW, path2: RPathW, more: RPathW*): _SetDefaultFor = setDefaultFor((path1, path2, more))
  def setDefaultFor(sel: SEL.SetDefault.Selector)               : _SetDefaultFor = new _SetDefaultFor(SEL.SetDefault.resolve(sel))

  def setDefaultConditionally(target: KeyW)                        : _SetDefaultConditionally2 = setDefaultConditionally(_.explicit(target))
  def setDefaultConditionally(selector: SEL.SetDefaultFor.Selector): _SetDefaultConditionally2 = new _SetDefaultConditionally2(SEL.SetDefaultFor.resolve(selector))

    // ---------------------------------------------------------------------------
    final class _SetDefaultFor private[common] (targets: TqRPathz) {
      def asValue[T: WTT](defaultValue: => T): Self2 =
        SetDefaultValueFor(ttqrpathz1(typeNode[T].wrapInOption)(targets), defaultValue) }

    // ---------------------------------------------------------------------------
    final class _SetDefaultConditionally2 private[common](target: TqKey) {
      def asValue[T: WTT](newValue: T) = new _AsValue[T](newValue, typeNode[T])

        // ---------------------------------------------------------------------------
        final class _AsValue[T] private[common] (newValue: T, node: TypeNode) {
          def ifValueFor(reference: KeyW)                  = new _IfValueFor1(reference)
          def ifValueFor[U: WTT](reference: IfValueFor[U]) = new _IfValueFor2(reference)

          // ---------------------------------------------------------------------------
          final class _IfValueFor1 private[common] (reference: KeyW) {
            def is(referenceValue: Any): Self2 =
              SetDefaultConditionally2Whatever(reference.value, target, referenceValue, node, newValue) }

          // ---------------------------------------------------------------------------
          final class _IfValueFor2[U: WTT] private[common] (reference: IfValueFor[U]) {
            def is     (value: Any)        : Self2 = matches(_ == value)
            def matches(pred: U => Boolean): Self2 =
              SetDefaultConditionally2(TSL.IfValueFor.resolve2(reference), target, pwrap(pred), node, newValue) } } }

  // ===========================================================================
  // convert

  def convert(x : RPathW)                            = new _Convert(RPathWz.from(x))
  def convert(x1: RPathW, x2: RPathW, more: RPathW*) = new _Convert(x1, x2, more)
  def convert(xs: RPathWz)                           = new _Convert(xs)

  def convert(sel: SEL.Convert.Selector) = new _Convert(SEL.Convert.resolve(sel))

    // ===========================================================================
    private[heads] def convert(target: TqRPathz) = new _Convert(target)
    class _Convert(target: TqRPathz) {

      /** can't call it toString */
      def toStr     : Self2 = ConvertToString(target)

      def toInt     : Self2 = ConvertToInt   (target)
      def toDouble  : Self2 = ConvertToDouble(target)

      def toBoolean : Self2 = toBoolean("true", "false")
      def toFlag    : Self2 = toFlag   ("true")

      def toBoolean        [T : WTT]              (trueValue: T, falseValue: T): Self2 = ConvertToBoolean        (target, trueValue, falseValue)
      def toOptionalBoolean[T : WTT](nullValue: T)(trueValue: T, falseValue: T): Self2 = ConvertToOptionalBoolean(target, trueValue, falseValue, nullValue)

      def toStrictFlag[T : WTT](trueValue: T                 ): Self2 = toFlag(trueValue, strict = true)
      def toFlag      [T : WTT](trueValue: T                 ): Self2 = toFlag(trueValue, strict = false)
      def toFlag      [T : WTT](trueValue: T, strict: Boolean): Self2 = ConvertToFlag(target, trueValue, strict)

      def toEnum(value1: EnumStringValue, more: EnumStringValue*): Self2 = toEnum((value1 +: more).map(EnumValue.apply))
      def toEnum(values: Seq[EnumValue])                         : Self2 = ConvertToEnum(target,  values.toList)

      // ---------------------------------------------------------------------------
      // TODO: keep these within convert?

      def toOptional                    : Self2 = ToOptional(target, strict = false)
      def toOptional   (strict: Boolean): Self2 = ToOptional(target, strict)
      def    toRequired                 : Self2 = ToRequired   (target, strict = false)
      def    toRequired(strict: Boolean): Self2 = ToRequired   (target, strict)

      // TODO: t230919102023 only on headM
      def toSingle                      : Self2 = ToSingle(target, strict = false) // aka force one
      def toSingle     (strict: Boolean): Self2 = ToSingle(target, strict)         // aka force one

      // TODO: t230919102023 only on headO
      def    toMultiple                 : Self2 = ToMultiple   (target, strict = false) // aka in seq
      def    toMultiple(strict: Boolean): Self2 = ToMultiple   (target, strict)         // aka in seq

      def toOneAndOnlyOne: Self2 = convert(target).toRequired.convert(target).toSingle
    }

  // ===========================================================================
  // translate

  def translate(x: RPathW ) = new _Translate(x)
  def translate(x: RPathWz) = new _Translate(x)

  def translate(x1: RPathW, x2: RPathW, more: RPathW*) = new _Translate(x1, x2 ,more)

  def translate(sel: SEL.Translate.Selector) = new _Translate(SEL.Translate.resolve(sel))

  def translateAll[K: WTT, V: WTT](entries: (KeyW, Map[K, V])*): Self2 = ??? // TODO, see 210117125042@w

    // ---------------------------------------------------------------------------
    class _Translate(target: TqRPathz) {
      /** strict = all values are translated, therefore type can change */
      def usingStrict[O: WTT, D : WTT](entry1: (O, D), more: (O, D)*): Self2 = usingStrict(entry1 +: more)

      /** strict = all values are translated, therefore type can change */
      def usingStrict[O: WTT, D : WTT](mapping: Map[O, D]           ): Self2 = usingStrict(mapping.toList)

      // ---------------------------------------------------------------------------
      def using       [O: WTT, D : WTT](entry1: (O, D), more: (O, D)*): Self2 = usingLenient(entry1 +: more)
      def usingLenient[O: WTT, D : WTT](entry1: (O, D), more: (O, D)*): Self2 = usingLenient(entry1 +: more)
      def using       [O: WTT, D : WTT](mapping: Map[O, D]           ): Self2 = usingLenient(mapping.toList)
      def usingLenient[O: WTT, D : WTT](mapping: Map[O, D]           ): Self2 = usingLenient(mapping.toList)

      // ---------------------------------------------------------------------------
      /** strict = all values are translated, therefore type can change */
      def usingStrict [O: WTT, D : WTT](entries: Seq[(O, D)]): Self2 = self2 :+
        Translate(ttqrpathz1(typeNode[O])(target), typeNode[D], true , entries)

      def usingLenient[O: WTT, D : WTT](entries: Seq[(O, D)]): Self2 = self2 :+
        Translate(ttqrpathz1(typeNode[O])(target), typeNode[D], false, entries) }

  // ===========================================================================
  // split

  def split(x : RPathW)                           : _Split = new _Split(RPathWz.from(x))
  def split(x1: RPathW, x2: RPathW, more: RPathW*): _Split = new _Split(x1, x2, more)
  def split(xs: RPathWz)                          : _Split = new _Split(xs)

    // ---------------------------------------------------------------------------
    class _Split(paths: RPathWz) {
      /** accounts for escaping */ def byXsv(sep: Char): Self2 = by(StringSplitterFunction(_.splitXsv(sep)))
      /** accounts for escaping */ def byCsv           : Self2 = by(StringSplitterFunction(_.splitXsv(',' )))
      /** accounts for escaping */ def byTsv           : Self2 = by(StringSplitterFunction(_.splitXsv('\t')))

      // ---------------------------------------------------------------------------
      def by(splitter: StringSplitter): Self2 = self2 :+ Split(paths.rpathz, splitter)
    }

  // ===========================================================================
  // swap

  def swapEntries(                key1: KeyW, key2: KeyW): Self2 = self2 :+ new SingleSwap(None                , key1.value, key2.value)
  def swapEntries(parent: RPathW)(key1: KeyW, key2: KeyW): Self2 = self2 :+ new SingleSwap(parent.value.in.some, key1.value, key2.value)

  def swapEntries(pair1: KeyPair, more: KeyPair*): Self2 = self2 :+ new MultiSwap(pair1 +: more)
  def swapEntries(pairs: Seq[KeyPair])           : Self2 = self2 :+ new MultiSwap(pairs)

  // ===========================================================================
  // copy

  def copyEntry(target: RPathW) = new _Copy(target.value)

    // ---------------------------------------------------------------------------
    class _Copy(target: RPath) {
      def asTmp: Self2 = as(_tmp)
      def as(newKey : KeyW)             : Self2 = self2 :+ new CopyEntries(target,  newKey.value.in.seq)
      def as(newKey1: KeyW, more: KeyW*): Self2 = self2 :+ new CopyEntries(target, (newKey1 +: more).map(_.value)) }

  // ===========================================================================
  // zip strings; eg: {"f": "a,b,c", "g": "1,2,3"} -> {"p": [{"f": "a", "g": "1"}, ...]}

  // TODO:
  // - t210109142621 - unzip operation (optionally with separator)
  // - t210110101000 - offer a "strict" zip (if expect no missing fields)
  // - selection (see t210110094731)?

  def zipStrings(key1: RenW, key2: RenW, more: RenW*): _ZipStrings = zipStrings((key1, key2, more))
  def zipStrings(keys: RenWz)                        : _ZipStrings = new _ZipStrings(keys.renz)

    // ---------------------------------------------------------------------------
    class _ZipStrings(keys: Renz) {
      def splitBy(commonSeparator: Separator) = new __ZipStrings(commonSeparator)

        // ---------------------------------------------------------------------------
        class __ZipStrings(commonSeparator: Separator) {
          def underNewKey(newNestingKey: KeyW): Self2 = self2 :+
            ZipStrings(keys, commonSeparator, newNestingKey.value) } }

  // ===========================================================================
  // notes:
  // - TODO: t210124100009 - odd suffices (1a, 2z, ...), but not easy to find meaningful names here so for now...

  /** eg for: {"f": ["foo", "3"]} */
  def deserialize1z(targetKey: RenW) = new _Deserialize1z(targetKey.value)

  /** eg for: {"f": "foo|3"} or {"f": ["foo|3", "bar|4"]} */
  def deserialize1a(targetKey: RenW) = new _Deserialize1a(targetKey.value)

  /** eg for: {"f": "foo|3,bar|4"} */
  def deserialize1b(targetKey: RenW) = new _Deserialize1b(targetKey.value)

  // ---------------------------------------------------------------------------
  // TODO: handle escaping?

  /** eg for: {"f": ["str=foo", "int=3"]} */
  def deserialize2z(targetKey: RenW) = new _Deserialize2z(targetKey.value)

  /** eg for: {"f": "str=foo|int=3"}  or {"f": [ "str=foo|int=3", "str=..." ]} */
  def deserialize2a(targetKey: RenW) = new _Deserialize2a(targetKey.value)

  /** eg for: {"f": "str=foo;int=3,str=bar;int=4,str=baz;int=5"} */
  def deserialize2b(targetKey: RenW) = new _Deserialize2b(targetKey.value)

    // ===========================================================================
    class _Deserialize1z(targetKey: Ren) {
          def asNewKeys[E <: EnumEntry : WTT]   : Self2 = asNewKeys(typeNode[E].flattenedEnumValueNames)
          def asNewKeys(key1: KeyW, more: KeyW*): Self2 = asNewKeys((key1, more))
          def asNewKeys(keys: KeyWz)            : Self2 = self2 :+
            Deserialize1z(targetKey, keys.keyz) }

    // ---------------------------------------------------------------------------
    class _Deserialize1a(targetKey: Ren) {
      def withSplitter(entriesSplitter: StringSplitter) = new _WithSplitter(entriesSplitter)
        final class _WithSplitter private[heads] (entriesSplitter: StringSplitter) {
          def asNewKeys[E <: EnumEntry : WTT]   : Self2 = asNewKeys(typeNode[E].flattenedEnumValueNames)
          def asNewKeys(key1: KeyW, more: KeyW*): Self2 = asNewKeys((key1, more))
          def asNewKeys(keys: KeyWz)            : Self2 = self2 :+
            Deserialize1a(targetKey, entriesSplitter, keys.keyz) } }

    // ---------------------------------------------------------------------------
    class _Deserialize1b(targetKey: Ren) {
      def withSplitters(arraySplitter: StringSplitter, entriesSplitter: StringSplitter) = new _WithSplitters(arraySplitter, entriesSplitter)
        final class _WithSplitters private[heads] (arraySplitter: StringSplitter, entriesSplitter: StringSplitter) {
          def asNewKeys[E <: EnumEntry : WTT]   : Self2 = asNewKeys(typeNode[E].flattenedEnumValueNames)
          def asNewKeys(key1: KeyW, more: KeyW*): Self2 = asNewKeys((key1, more))
          def asNewKeys(keys: KeyWz)            : Self2 = self2 :+
            Deserialize1b(targetKey, arraySplitter, entriesSplitter, keys.keyz) } }

    // ===========================================================================
    final class _Deserialize2z(targetKey: Ren) {
        def withSplitter(entrySplitter: StringSplitter) = new _WithSplitter(entrySplitter)
          final class _WithSplitter private[heads] (entrySplitter: StringSplitter) {
            def asNewKeys[E <: EnumEntry : WTT]   : Self2 = asNewKeys(typeNode[E].flattenedEnumValueNames)
            def asNewKeys(key1: KeyW, more: KeyW*): Self2 = asNewKeys((key1, more))
            def asNewKeys(keys: KeyWz)            : Self2 = self2 :+
              Deserialize2z(targetKey, entrySplitter, keys.keyz) } }

      // ---------------------------------------------------------------------------
      final class _Deserialize2a(targetKey: Ren) {
        def withSplitters(entriesSplitter: StringSplitter, entrySplitter: StringSplitter) = new _WithSplitters(entriesSplitter, entrySplitter)
          final class _WithSplitters private[heads] (entriesSplitter: StringSplitter, entrySplitter: StringSplitter) {
            def asNewKeys[E <: EnumEntry : WTT]   : Self2 = asNewKeys(typeNode[E].flattenedEnumValueNames)
            def asNewKeys(key1: KeyW, more: KeyW*): Self2 = asNewKeys((key1, more))
            def asNewKeys(keys: KeyWz)            : Self2 = self2 :+
              Deserialize2a(targetKey, entriesSplitter, entrySplitter, keys.keyz) } }

      // ---------------------------------------------------------------------------
      final class _Deserialize2b(targetKey: Ren) {
        def withSplitters(arraySplitter: StringSplitter, entriesSplitter: StringSplitter, entrySplitter: StringSplitter) = new _WithSplitters(arraySplitter, entriesSplitter, entrySplitter)
          final class _WithSplitters private[heads] (arraySplitter: StringSplitter, entriesSplitter: StringSplitter, entrySplitter: StringSplitter) {
            def asNewKeys[E <: EnumEntry : WTT]   : Self2 = asNewKeys(typeNode[E].flattenedEnumValueNames)
            def asNewKeys(key1: KeyW, more: KeyW*): Self2 = asNewKeys((key1, more))
            def asNewKeys(keys: KeyWz)            : Self2 = self2 :+
              Deserialize2b(targetKey, arraySplitter, entriesSplitter, entrySplitter, keys.keyz) } }

  // ===========================================================================
  // this is the bare minimum (fifteen pieces of flair)
  // TODO: t210608090944 - allow selection + customization
  def unpivot(key1: KeyW, more: KeyW*): Self2 = self2 :+ actions.ActionsOthers.Unpivot(Keyz.from(key1, more))

  // ---------------------------------------------------------------------------
  // TODO: t220914144753 - generalize as unpivot of some keys
  def unpivotOneItem(key1: KeyW, key2: KeyW) = new _UnpivotOneItem(key1, key2)
   final class _UnpivotOneItem private[heads] (key1: KeyW, key2: KeyW) {
    def withValue(targetValue: Any): Self2 = self2 :+
      actions.ActionsOthers.UnpivotOneItem(key1.value, key2.value, targetValue.toString) }
}

// ===========================================================================
