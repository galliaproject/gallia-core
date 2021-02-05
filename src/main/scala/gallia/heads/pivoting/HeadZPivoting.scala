package gallia.heads.pivoting

import aptus.Separator

import gallia._
import gallia.actions.ActionsOthers._

// ===========================================================================
trait HeadZPivoting { self: HeadZ =>
  import gallia.heads.common.Pivot

  // ---------------------------------------------------------------------------
  //TODO: t210110094829 - opaque nesting: accept Obj as value (standalone version), so may not provide newKeys when unwanted
  //TODO: t210117192141 - enum counterpart to provide newKeys, eg def asNewKeys[MyEnum]

  // ===========================================================================
  @Max5 protected val _pivot = new PivotingHelper(self)

    def pivot                                             (x1: KPathW                                                               ) = _pivot.pivot1(_._explicit(x1))
    def pivot[O1: WTT                                    ](x1: Pivot[O1]                                                            ) = _pivot.pivot1(x1)
    def pivot[O1: WTT, O2: WTT                           ](x1: Pivot[O1], x2: Pivot[O2]                                             ) = _pivot.pivot2(x1, x2)
    def pivot[O1: WTT, O2: WTT, O3: WTT                  ](x1: Pivot[O1], x2: Pivot[O2], x3: Pivot[O3]                              ) = _pivot.pivot3(x1, x2, x3)
    def pivot[O1: WTT, O2: WTT, O3: WTT, O4: WTT         ](x1: Pivot[O1], x2: Pivot[O2], x3: Pivot[O3], x4: Pivot[O4]               ) = _pivot.pivot4(x1, x2, x3, x4)
    def pivot[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](x1: Pivot[O1], x2: Pivot[O2], x3: Pivot[O3], x4: Pivot[O4], x5: Pivot[O5]) = _pivot.pivot5(x1, x2, x3, x4, x5)

  // ===========================================================================
  def unarrayBy(key1: KeyW)                          = new _UnarrayBy1(Keyz.from(key1))

  def unarrayBy(key1: KeyW, key2: KeyW, more: KeyW*) = new _UnarrayByN(Keyz.from(key1, key2, more))
  def unarrayBy(keys: KeyWz)                         = new _UnarrayByN(keys.keyz)

    // ---------------------------------------------------------------------------
    class _UnarrayBy1(keyKeys: Keyz) {
      def asNewKeys(x1: KeyW, xs: KeyW*) = zu(UnarrayBy(Keyz.from(x1, xs), keyKeys, sep = null /* ignored by design */))
      def asNewKeys(xs: KeyWz)           = zu(UnarrayBy(xs.keyz          , keyKeys, sep = null /* ignored by design */)) }


    // ---------------------------------------------------------------------------
    class _UnarrayByN(keyKeys: Keyz) {
      def asNewKeys(x1: KeyW, xs: KeyW*) = new __UnarrayByN(Keyz.from(x1, xs))
      def asNewKeys(xs: KeyWz)           = new __UnarrayByN(xs.keyz)

      // ---------------------------------------------------------------------------
      class __UnarrayByN protected[HeadZPivoting] (newKeys: Keyz) {
        def withDefaultKeySeparator = withKeySeparator("_")

        def withKeySeparator(value: Separator) =
          zu(UnarrayBy(newKeys, keyKeys, sep = value)) } }

  // ---------------------------------------------------------------------------
  def unarrayEntries(key1: KeyW) = new _UnarrayEntries1(Keyz.from(key1))

  def unarrayEntries(key1: KeyW, key2: KeyW, more: KeyW*) = new _UnarrayEntriesN(Keyz.from(key1, key2, more))
  def unarrayEntries(keys: KeyWz)                         = new _UnarrayEntriesN(keys.keyz)

    // ---------------------------------------------------------------------------
    class _UnarrayEntries1(keyKeys: Keyz) {
      def asNewKeys(x1: KeyW, xs: KeyW*) = new __UnarrayEntries1(Keyz.from(x1, xs))
      def asNewKeys(xs: KeyWz)           = new __UnarrayEntries1(xs.keyz)

      // ---------------------------------------------------------------------------
      class __UnarrayEntries1 protected[HeadZPivoting] (newKeys: Keyz) {
        def valueKey(y: KeyW): HeadU =
          zu(UnarrayEntries(newKeys, keyKeys, separator = null /* ignored by design */, y.value)) }
    }

    // ---------------------------------------------------------------------------
    class _UnarrayEntriesN(keyKeys: Keyz) {
      def asNewKeys(x1: KeyW, xs: KeyW*) = new __UnarrayEntriesN(Keyz.from(x1, xs))
      def asNewKeys(xs: KeyWz)           = new __UnarrayEntriesN(xs.keyz)

      // ---------------------------------------------------------------------------
      class __UnarrayEntriesN protected[HeadZPivoting] (newKeys: Keyz) {
        def withDefaultKeySeparator = withKeySeparator("_")

        def withKeySeparator(value: Separator) =
          new { def valueKey(y: KeyW): HeadU =
            zu(UnarrayEntries(newKeys, keyKeys, separator = value, y.value)) } }
    }

}

// ===========================================================================
