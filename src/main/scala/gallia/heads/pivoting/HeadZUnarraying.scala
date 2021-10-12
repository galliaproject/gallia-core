package gallia
package heads.pivoting

import aptus.Separator

import actions.ActionsOthers._

// ===========================================================================
@deprecated trait HeadZUnarraying /* "pivone" */ { self: HeadZ =>
  //val Pivone = gallia.heads.SEL.Pivone

  // ---------------------------------------------------------------------------
  //TODO: t210110094829 - opaque nesting: accept Obj as value (standalone version), so may not provide newKeys when unwanted
  //TODO: t210117192141 - enum counterpart to provide newKeys, eg def asNewKeys[MyEnum]

  // ===========================================================================
  @deprecated("use pivot now (\"pivone\")") def unarrayEntries(key1: KeyW) = new _UnarrayEntries1(Keyz.from(key1))

  @deprecated("use pivot now (\"pivone\")") def unarrayEntries(key1: KeyW, key2: KeyW, more: KeyW*) = new _UnarrayEntriesN(Keyz.from(key1, key2, more))
  @deprecated("use pivot now (\"pivone\")") def unarrayEntries(keys: KeyWz)                         = new _UnarrayEntriesN(keys.keyz)

    // ---------------------------------------------------------------------------
    class _UnarrayEntries1(keyKeys: Keyz) {
      def asNewKeys(x1: KeyW, xs: KeyW*) = new __UnarrayEntries1(Keyz.from(x1, xs))
      def asNewKeys(xs: KeyWz)           = new __UnarrayEntries1(xs.keyz)

      // ---------------------------------------------------------------------------
      class __UnarrayEntries1 protected[HeadZUnarraying] (newKeys: Keyz) {
        def valueKey(y: KeyW): HeadU = zu(UnarrayEntries0(newKeys, keyKeys, separator = null /* ignored by design */, y.value)) } }

    // ---------------------------------------------------------------------------
    class _UnarrayEntriesN(keyKeys: Keyz) {
      def asNewKeys(x1: KeyW, xs: KeyW*) = new __UnarrayEntriesN(Keyz.from(x1, xs))
      def asNewKeys(xs: KeyWz)           = new __UnarrayEntriesN(xs.keyz)

      // ---------------------------------------------------------------------------
      // TODO: t210228112738 - keep these or force manual fuse ahead of time?      
      class __UnarrayEntriesN protected[HeadZUnarraying] (newKeys: Keyz) {
        def withDefaultKeySeparator = withKeySeparator("_")
        def withKeySeparator(value: Separator) =
          new { def valueKey(y: KeyW): HeadU = zu(UnarrayEntries0(newKeys, keyKeys, separator = value, y.value)) } }
    }

  // ===========================================================================
  // nt210303104417 - may not be kept altogether
    
  // TODO: t210303103728 - meta and data disagree
  @deprecated("use pivot now (\"pivone\")") def unarrayBy0(key1: KeyW)                          = new _UnarrayBy1(Keyz.from(key1))

  @deprecated("use pivot now (\"pivone\")") def unarrayBy0(key1: KeyW, key2: KeyW, more: KeyW*) = new _UnarrayByN(Keyz.from(key1, key2, more))
  @deprecated("use pivot now (\"pivone\")") def unarrayBy0(keys: KeyWz)                         = new _UnarrayByN(keys.keyz)

    // ---------------------------------------------------------------------------
    class _UnarrayBy1(keyKeys: Keyz) {
      def asNewKeys(x1: KeyW, xs: KeyW*) = zu(UnarrayBy0(Keyz.from(x1, xs), keyKeys, sep = null /* ignored by design */))
      def asNewKeys(xs: KeyWz)           = zu(UnarrayBy0(xs.keyz          , keyKeys, sep = null /* ignored by design */)) }

    // ---------------------------------------------------------------------------
    class _UnarrayByN(keyKeys: Keyz) {
      def asNewKeys(x1: KeyW, xs: KeyW*) = new __UnarrayByN(Keyz.from(x1, xs))
      def asNewKeys(xs: KeyWz)           = new __UnarrayByN(xs.keyz)

      // ---------------------------------------------------------------------------
      // TODO: t210228112738 - keep these or force manual fuse ahead of time?
      class __UnarrayByN protected[HeadZUnarraying] (newKeys: Keyz) {
        def withDefaultKeySeparator            = withKeySeparator("_")
        def withKeySeparator(value: Separator) = zu(UnarrayBy0(newKeys, keyKeys, sep = value)) } }
    
}

// ===========================================================================
