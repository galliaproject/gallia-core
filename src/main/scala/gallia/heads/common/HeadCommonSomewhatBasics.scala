package gallia.heads.common

import aptus.{Anything_, String_}

import aptus.Separator

import gallia._
import gallia.actions.ActionsUUSomewhatBasics._
import gallia.actions.ActionsUUConverts._
import gallia.actions.ActionsUUUntuplify._
import gallia.domain._
import gallia.target._
import gallia.target.utils.TypedTargetQueryUtils._

// ===========================================================================
trait HeadCommonSomewhatBasics[F <: HeadCommon[F]] { _: HeadCommon[F] =>

  // ---------------------------------------------------------------------------
  // remove conditionally
  // note: 210111134300 - may need to name the whatever version differently, else creates  lots of problems
  //   most of the time, we expect the Whatever version to be used though...

  def removeIfValueFor(f : RPathW )                           = new _RemoveWhateverIf(RPathWz.from(f))
  def removeIfValueFor(f : RPathWz)                           = new _RemoveWhateverIf(f)
  def removeIfValueFor(f1: RPathW, f2: RPathW, more: RPathW*) = new _RemoveWhateverIf(f1, f2 ,more) // keep?

  // ---------------------------------------------------------------------------
  def removeIfValueFor[T: WTT](f: RemoveIf[T]) = new _RemoveConditionally[T](TSL.RemoveIf.resolves(f)) // TODO: accept renaming

    // TODO: t210111134021 - forbid is(None)

    // ===========================================================================
    final class _RemoveConditionally[T: WTT] private[HeadCommonSomewhatBasics](target: TtqRPathz) {
      def is(value: T): Self2 =
        if (node[T].isWhatever) matches(_ == value.asInstanceOf[WV].any)
        else                    matches(_ == value)

      // ---------------------------------------------------------------------------
      def matches(pred: T => Boolean): Self2 = self2 :+ RemoveConditionally(target, pwrap(pred)) }

    // ===========================================================================
    final class _RemoveWhateverIf private[HeadCommonSomewhatBasics](target: TqRPathz) {
      def is[T: WTT](a: T): Self2 = self2 :+ RemoveConditionallyWhatever(target, a) }

  // ===========================================================================
  // set default

  // TODO: t201005102404 - for multiple: offer backfill/forward fill
  // TODO: t210129093607 - offer a setDefaults('foo -> 1, 'bar -> "baz", ...)

  def setDefaultFor(path : RPathW ): _SetDefaultFor = setDefaultFor(RPathWz.from(path))
  def setDefaultFor(paths: RPathWz): _SetDefaultFor = setDefaultFor(TargetQueryUtils.tqqpathz(paths.qpathz))

  def setDefaultFor(path1: RPathW, path2: RPathW, more: RPathW*): _SetDefaultFor = setDefaultFor((path1, path2, more))
  def setDefaultFor(sel: SEL.SetDefault.Selector)               : _SetDefaultFor = new _SetDefaultFor(SEL.SetDefault.resolve(sel))

    // ---------------------------------------------------------------------------
    private[heads] def setDefaultFor(target: TqRPathz) = new _SetDefaultFor(target)
    final class _SetDefaultFor(targets: TqRPathz){
      def asValue[T: WTT](defaultValue: => T): Self2 =
        self2 :+ SetDefault(ttqqpathz1[Option[T]](targets), defaultValue) }

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
      def toStr     : Self2 = self2 :+ ConvertToString(target)

      def toInt     : Self2 = self2 :+ ConvertToInt   (target)
      def toDouble  : Self2 = self2 :+ ConvertToDouble(target)

      def toBoolean : Self2 = toBoolean("true", "false")
      def toFlag    : Self2 = toFlag   ("true")

      def toBoolean        [T : WTT]              (trueValue: T, falseValue: T): Self2 = self2 :+ ConvertToBoolean        (target, trueValue, falseValue)
      def toOptionalBoolean[T : WTT](nullValue: T)(trueValue: T, falseValue: T): Self2 = self2 :+ ConvertToOptionalBoolean(target, trueValue, falseValue, nullValue)

      def toStrictFlag[T : WTT](trueValue: T                 ): Self2 = toFlag(trueValue, strict = true)
      def toFlag      [T : WTT](trueValue: T                 ): Self2 = toFlag(trueValue, strict = false)
      def toFlag      [T : WTT](trueValue: T, strict: Boolean): Self2 = self2 :+ ConvertToFlag(target, trueValue, strict)

      // ---------------------------------------------------------------------------
      // TODO: keep these within convert?

      def toNonRequired                 : Self2 = self2 :+ ToNonRequired(target, strict = false)
      def toNonRequired(strict: Boolean): Self2 = self2 :+ ToNonRequired(target, strict)
      def    toRequired                 : Self2 = self2 :+ ToRequired   (target, strict = false)
      def    toRequired(strict: Boolean): Self2 = self2 :+ ToRequired   (target, strict)

      def toNonMultiple                 : Self2 = self2 :+ ToNonMultiple(target, strict = false) // aka force one
      def toNonMultiple(strict: Boolean): Self2 = self2 :+ ToNonMultiple(target, strict)         // aka force one
      def    toMultiple                 : Self2 = self2 :+ ToMultiple   (target, strict = false) // aka in seq
      def    toMultiple(strict: Boolean): Self2 = self2 :+ ToMultiple   (target, strict)         // aka in seq

      def toOneAndOnlyOne: Self2 = convert(target).toRequired.convert(target).toNonMultiple
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
      def usingStrict [O: WTT, D : WTT](entries: Seq[(O, D)]): Self2 = self2 :+ new Translate(ttqqpathz1[O](target), node[D], true , entries)
      def usingLenient[O: WTT, D : WTT](entries: Seq[(O, D)]): Self2 = self2 :+ new Translate(ttqqpathz1[O](target), node[D], false, entries)
    }

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
      def by(splitter: StringSplitter): Self2 = self2 :+ Split(paths.qpathz, splitter)
    }

  // ===========================================================================
  // swap

  def swapEntries(                key1: KeyW, key2: KeyW): Self2 = self2 :+ new SingleSwap(None                , key1.value, key2.value)
  def swapEntries(parent: RPathW)(key1: KeyW, key2: KeyW): Self2 = self2 :+ new SingleSwap(parent.value.as.some, key1.value, key2.value)

  def swapEntries(pair1: KeyWPair, more: KeyWPair*): Self2 = self2 :+ new MultiSwap(pair1 +: more)
  def swapEntries(pairs: Seq[KeyWPair])            : Self2 = self2 :+ new MultiSwap(pairs)

  // ===========================================================================
  // copy

  def copyEntry(target: RPathW) = new _Copy(target.value)

    // ---------------------------------------------------------------------------
    class _Copy(target: RPath) {
      def as(newKey : KeyW)             : Self2 = self2 :+ new CopyEntries(target,  newKey.value.as.seq)
      def as(newKey1: KeyW, more: KeyW*): Self2 = self2 :+ new CopyEntries(target, (newKey1 +: more).map(_.value)) }

  // ===========================================================================
  // zip strings

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
  def untuplify1z(targetKey: RenW) = new _Untuplify1z(targetKey.value)

  /** eg for: {"f": "foo|3"} or {"f": ["foo|3", "bar|4"]} */
  def untuplify1a(targetKey: RenW) = new _Untuplify1a(targetKey.value)

  /** eg for: {"f": "foo|3,bar|4"} */
  def untuplify1b(targetKey: RenW) = new _Untuplify1b(targetKey.value)

  // ---------------------------------------------------------------------------
  // TODO: handle escaping?

  /** eg for: {"f": ["str=foo", "int=3"]} */
  def untuplify2z(targetKey: RenW) = new _Untuplify2z(targetKey.value)

  /** eg for: {"f": "str=foo|int=3"}  or {"f": [ "str=foo|int=3", "str=..." ]} */  
  def untuplify2a(targetKey: RenW) = new _Untuplify2a(targetKey.value)

  /** eg for: {"f": "str=foo;int=3,str=bar;int=4,str=baz;int=5"} */
  def untuplify2b(targetKey: RenW) = new _Untuplify2b(targetKey.value)

    // ===========================================================================
    class _Untuplify1z(targetKey: Ren) {
          def asNewKeys(key1: KeyW, more: KeyW*): Self2 = asNewKeys((key1, more))
          def asNewKeys(keys: KeyWz)            : Self2 = self2 :+
            Untuplify1z(targetKey, keys.keyz) }

    // ---------------------------------------------------------------------------
    class _Untuplify1a(targetKey: Ren) {
      def withSplitter(entriesSplitter: StringSplitter) = new {
          def asNewKeys(key1: KeyW, more: KeyW*): Self2 = asNewKeys((key1, more))
          def asNewKeys(keys: KeyWz)            : Self2 = self2 :+
            Untuplify1a(targetKey, entriesSplitter, keys.keyz) } }

    // ---------------------------------------------------------------------------
    class _Untuplify1b(targetKey: Ren) {
      def withSplitters(arraySplitter: StringSplitter, entriesSplitter: StringSplitter) = new {
          def asNewKeys(key1: KeyW, more: KeyW*): Self2 = asNewKeys((key1, more))
          def asNewKeys(keys: KeyWz)            : Self2 = self2 :+
            Untuplify1b(targetKey, arraySplitter, entriesSplitter, keys.keyz) } }

    // ===========================================================================
    final class _Untuplify2z(targetKey: Ren) {
        def withSplitter(entrySplitter: StringSplitter) = new {
            def asNewKeys(key1: KeyW, more: KeyW*): Self2 = asNewKeys((key1, more))
            def asNewKeys(keys: KeyWz)            : Self2 = self2 :+
              Untuplify2z(targetKey, entrySplitter, keys.keyz) } }

      // ---------------------------------------------------------------------------
      final class _Untuplify2a(targetKey: Ren) {
        def withSplitters(entriesSplitter: StringSplitter, entrySplitter: StringSplitter) = new {
            def asNewKeys(key1: KeyW, more: KeyW*): Self2 = asNewKeys((key1, more))
            def asNewKeys(keys: KeyWz)            : Self2 = self2 :+
              Untuplify2a(targetKey, entriesSplitter, entrySplitter, keys.keyz) } }

      // ---------------------------------------------------------------------------
      final class _Untuplify2b(targetKey: Ren) {
        def withSplitters(arraySplitter: StringSplitter, entriesSplitter: StringSplitter, entrySplitter: StringSplitter) = new {
            def asNewKeys(key1: KeyW,  more: KeyW*): Self2 = asNewKeys((key1, more))
            def asNewKeys(keys: KeyWz)             : Self2 = self2 :+
              Untuplify2b(targetKey, arraySplitter, entriesSplitter, entrySplitter, keys.keyz) } }

  // ===========================================================================
  // this is the bare minimum (fifteen pieces of flair)
  // TODO: t210608090944 - allow selection + customization
  def unpivot(key1: KeyW, more: KeyW*): Self2 = self2 :+ actions.ActionsOthers.Unpivot(Keyz.from(key1, more))

}

// ===========================================================================
