package gallia
package atoms

import domain._
import atoms.utils.SortWrapping._

// ===========================================================================
@Max5
object AtomsZZSorting { import utils.SuperMetaPair

  // ---------------------------------------------------------------------------
  case class _SortUnsafe[T: WTT](f: Obj => T, meta: SuperMetaPair[T]) extends AtomZZ { def naive(z: Objs) =
    z.sortUnsafe(f, meta) }

  // ===========================================================================
  case class _SortByAll(c: Cls, pair: SortingPair) extends AtomZZ { def naive(z: Objs) =
      z.sortByAll(c, pair) }

    // ---------------------------------------------------------------------------
    case class _SortBy1[K]         (c: Cls, sortWrapper: SortWrapper1[K])          extends AtomZZ { def naive(z: Objs) = z.sort(c, sortWrapper) }
    case class _SortBy2[K1, K2]    (c: Cls, sortWrapper: SortWrapper2[K1, K2])     extends AtomZZ { def naive(z: Objs) = z.sort(c, sortWrapper) }
    case class _SortBy3[K1, K2, K3](c: Cls, sortWrapper: SortWrapper3[K1, K2, K3]) extends AtomZZ { def naive(z: Objs) = z.sort(c, sortWrapper) }
    // _SortByN attempt: see 210116115250@w

  // ===========================================================================
  case class _CustomSort1[T](ori: PathPair1, f: _ff11, meta: SuperMetaPair[T]) extends AtomZZ { def naive(z: Objs) = {
      val g: Obj => T = ori.lookup(_).pipe(f).asInstanceOf[T]
      z._modifyUnderlyingStreamer(_.sortBy(meta)(g)) } }

    // ---------------------------------------------------------------------------
    case class _CustomSort2[T](ori: PathPair2, f: _ff21, meta: SuperMetaPair[T]) extends AtomZZ { def naive(z: Objs) = {
      val g: Obj => T = ori.lookup(_).pipe(f.tupled).asInstanceOf[T]
      z._modifyUnderlyingStreamer(_.sortBy(meta)(g)) } }

}

// ===========================================================================
