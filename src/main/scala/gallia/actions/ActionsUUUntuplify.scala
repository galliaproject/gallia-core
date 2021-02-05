package gallia.actions

import gallia._
import gallia.atoms.AtomsUUUntuplify._

// ===========================================================================
object ActionsUUUntuplify {

  case class Untuplify1z(targetKey: Ren, keys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.untuplifyz(targetKey, keys)
        def atomuu             = _Untuplify1z(targetKey, keys) }

      // ---------------------------------------------------------------------------
      case class _Untuplify1z(targetKey: Ren, keys: Keyz) extends AtomUU { def naive(o: Obj) =
        untuplify1z(o, targetKey, keys) }

    // ---------------------------------------------------------------------------
    case class Untuplify1a(targetKey: Ren, entriesSplitter: StringSplitter, keys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.untuplifya(targetKey, keys)
        def atomuu             = _Untuplify1a(targetKey, entriesSplitter, keys) }

      // ---------------------------------------------------------------------------
      case class _Untuplify1a(targetKey: Ren, splitter: StringSplitter, keys: Keyz) extends AtomUU { def naive(o: Obj) =
        untuplify1a(o, targetKey, splitter, keys) }

    // ---------------------------------------------------------------------------
    case class Untuplify1b(targetKey: Ren, arraySplitter: StringSplitter, entriesSplitter: StringSplitter, keys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.untuplifyb(targetKey, keys)
        def atomuu             = _Untuplify1b(targetKey, arraySplitter, entriesSplitter, keys) }

      // ---------------------------------------------------------------------------
      case class _Untuplify1b(targetKey: Ren, arraySplitter: StringSplitter, /* entry */ splitter: StringSplitter, keys: Keyz) extends AtomUU { def naive(o: Obj) =
        untuplify1b(o, targetKey, arraySplitter, splitter, keys) }

  // ===========================================================================
  case class Untuplify2z(targetKey: Ren, entrySplitter: StringSplitter, newKeys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.untuplifyz(targetKey, newKeys)
        def atomuu             = _Untuplify2z(targetKey, entrySplitter, newKeys) }

      // ---------------------------------------------------------------------------
      case class _Untuplify2z(targetKey: Ren, entrySplitter: StringSplitter, newKeys: Keyz) extends AtomUU { def naive(o: Obj) =
        untuplify2z(targetKey)(entrySplitter)(newKeys.valueSet)(o) }

    // ---------------------------------------------------------------------------
    case class Untuplify2a(targetKey: Ren, entriesSplitter: StringSplitter, entrySplitter: StringSplitter, newKeys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.untuplifya(targetKey, newKeys)
        def atomuu             = _Untuplify2a(targetKey, entriesSplitter, entrySplitter, newKeys) }

      // ---------------------------------------------------------------------------
      case class _Untuplify2a(targetKey: Ren, entriesSplitter: StringSplitter, entrySplitter: StringSplitter, newKeys: Keyz) extends AtomUU { def naive(o: Obj) =
        untuplify2a(targetKey)(entriesSplitter, entrySplitter)(newKeys.valueSet)(o) }

    // ---------------------------------------------------------------------------
    case class Untuplify2b(targetKey: Ren, arraySplitter: StringSplitter, entriesSplitter: StringSplitter, entrySplitter: StringSplitter, newKeys: Keyz) extends ActionUUd with TodoV1 {
        def _meta(c: Cls): Cls = c.untuplifyb(targetKey, newKeys)
        def atomuu             = _Untuplify2b(targetKey, arraySplitter, entriesSplitter, entrySplitter, newKeys) }

      // ---------------------------------------------------------------------------
      case class _Untuplify2b(targetKey: Ren, arraySplitter: StringSplitter, entriesSplitter: StringSplitter, entrySplitter: StringSplitter, newKeys: Keyz) extends AtomUU { def naive(o: Obj) =
        untuplify2b(targetKey)(arraySplitter, entriesSplitter, entrySplitter)(newKeys.valueSet)(o) }
}

// ===========================================================================
