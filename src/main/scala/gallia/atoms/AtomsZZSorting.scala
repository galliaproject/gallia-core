package gallia.atoms

import scala.reflect.ClassTag
import aptus.Anything_

import gallia._
import gallia.domain._

// ===========================================================================
@gallia.Max5
object AtomsZZSorting {
  import gallia.atoms.utils.SuperMetaPair

  // ---------------------------------------------------------------------------
  @deprecated("use SuperMetaPair now?") private def ctag[T1, T2] = implicitly[ClassTag[(T1, T2)]]/* TODO: t201130102656 - check ok with spark */

  // ===========================================================================
  case class _SortUnsafe[T: ClassTag](f: Obj => T, ctag: ClassTag[T], ord: Ordering[T]) extends AtomZZ { def naive(z: Objs) =
       z._modifyUnderlyingStreamer(_.sortBy(ctag, ord)(f)) }

  // ===========================================================================
  case class _SortByAll(c: Cls, pair: SortingPair) extends AtomZZ { def naive(z: Objs) =
        z.sortBy(c, pair) }

      @deprecated case class __SortByAll(c: Cls, targets: KPathz, pairs: Either[SortingPair, Map[KPath, SortingPair]]) extends AtomZZ { def naive(z: Objs) =
        ??? } // TODO: may be too costly? have to use ObjOrdering.optionObjOrdering(c, pair))

    // ---------------------------------------------------------------------------
    case class _SortBy1[T](ori: PathPair1, meta: SuperMetaPair[T]) extends AtomZZ { def naive(z: Objs) =
        z._modifyUnderlyingStreamer(_.sortBy(meta.ctag, meta.ord)(ori.lookup(_).asInstanceOf[T])) }

      // ---------------------------------------------------------------------------
      case class _SortBy2[T1, T2](
              ori1: PathPair1, meta1: SuperMetaPair[T1],
              ori2: PathPair1, meta2: SuperMetaPair[T2])
            extends AtomZZ { def naive(z: Objs) = {
          val g: Obj => (T1, T2) = PathPair2(ori1, ori2).lookup(_).asInstanceOf[(T1, T2)]

          z._modifyUnderlyingStreamer(_.sortBy(
              ctag[T1, T2],
              Ordering.Tuple2(meta1.ord, meta2.ord))(g))
        }
      }

      // ---------------------------------------------------------------------------
      // _SortByN attempt: see 210116115250@w

  // ===========================================================================
  case class _CustomSort1[T](ori: PathPair1, f: _ff11, ctag: ClassTag[T], ord: Ordering[T]) extends AtomZZ { def naive(z: Objs) = {
      val g: Obj => T = ori.lookup(_).pipe(f).asInstanceOf[T]
      implicit val x = ctag; z._modifyUnderlyingStreamer(_.sortBy(ctag, ord)(g)) } }

    // ---------------------------------------------------------------------------
    case class _CustomSort2[T](ori: PathPair2, f: _ff21, ctag: ClassTag[T], ord: Ordering[T]) extends AtomZZ { def naive(z: Objs) = {
      val g: Obj => T = ori.lookup(_).pipe(f.tupled).asInstanceOf[T]
      z._modifyUnderlyingStreamer(_.sortBy(ctag, ord)(g)) } }

}

// ===========================================================================
