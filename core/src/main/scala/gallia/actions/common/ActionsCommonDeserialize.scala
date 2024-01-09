package gallia
package actions
package common

import atoms.common.AtomsCommonDeserialize._

// ===========================================================================
object ActionsCommonDeserialize {

  case class Deserialize1z(targetKey: Ren, keys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.deserializez(targetKey, keys)
        def atomuu             = _Deserialize1z(targetKey, keys) }

      // ---------------------------------------------------------------------------
      case class _Deserialize1z(targetKey: Ren, keys: Keyz) extends AtomUU { def naive(o: Obj) =
        deserialize1z(o, targetKey, keys) }

    // ---------------------------------------------------------------------------
    case class Deserialize1a(targetKey: Ren, entriesSplitter: StringSplitter, keys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.deserializea(targetKey, keys)
        def atomuu             = _Deserialize1a(targetKey, entriesSplitter, keys) }

      // ---------------------------------------------------------------------------
      case class _Deserialize1a(targetKey: Ren, splitter: StringSplitter, keys: Keyz) extends AtomUU { def naive(o: Obj) =
        deserialize1a(o, targetKey, splitter, keys) }

    // ---------------------------------------------------------------------------
    case class Deserialize1b(targetKey: Ren, arraySplitter: StringSplitter, entriesSplitter: StringSplitter, keys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.deserializeb(targetKey, keys)
        def atomuu             = _Deserialize1b(targetKey, arraySplitter, entriesSplitter, keys) }

      // ---------------------------------------------------------------------------
      case class _Deserialize1b(targetKey: Ren, arraySplitter: StringSplitter, /* entry */ splitter: StringSplitter, keys: Keyz) extends AtomUU { def naive(o: Obj) =
        deserialize1b(o, targetKey, arraySplitter, splitter, keys) }

  // ===========================================================================
  case class Deserialize2z(targetKey: Ren, entrySplitter: StringSplitter, newKeys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.deserializez(targetKey, newKeys)
        def atomuu             = _Deserialize2z(targetKey, entrySplitter, newKeys) }

      // ---------------------------------------------------------------------------
      case class _Deserialize2z(targetKey: Ren, entrySplitter: StringSplitter, newKeys: Keyz) extends AtomUU { def naive(o: Obj) =
        deserialize2z(targetKey)(entrySplitter)(newKeys.valueSet)(o) }

    // ---------------------------------------------------------------------------
    case class Deserialize2a(targetKey: Ren, entriesSplitter: StringSplitter, entrySplitter: StringSplitter, newKeys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.deserializea(targetKey, newKeys)
        def atomuu             = _Deserialize2a(targetKey, entriesSplitter, entrySplitter, newKeys) }

      // ---------------------------------------------------------------------------
      case class _Deserialize2a(targetKey: Ren, entriesSplitter: StringSplitter, entrySplitter: StringSplitter, newKeys: Keyz) extends AtomUU { def naive(o: Obj) =
        deserialize2a(targetKey)(entriesSplitter, entrySplitter)(newKeys.valueSet)(o) }

    // ---------------------------------------------------------------------------
    case class Deserialize2b(targetKey: Ren, arraySplitter: StringSplitter, entriesSplitter: StringSplitter, entrySplitter: StringSplitter, newKeys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.deserializeb(targetKey, newKeys)
        def atomuu             = _Deserialize2b(targetKey, arraySplitter, entriesSplitter, entrySplitter, newKeys) }

      // ---------------------------------------------------------------------------
      case class _Deserialize2b(targetKey: Ren, arraySplitter: StringSplitter, entriesSplitter: StringSplitter, entrySplitter: StringSplitter, newKeys: Keyz) extends AtomUU { def naive(o: Obj) =
        deserialize2b(targetKey)(arraySplitter, entriesSplitter, entrySplitter)(newKeys.valueSet)(o) }
}

// ===========================================================================
