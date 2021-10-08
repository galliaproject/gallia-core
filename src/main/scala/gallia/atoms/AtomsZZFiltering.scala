package gallia.atoms

import aptus.Anything_

import gallia._
import gallia.domain._

// ===========================================================================
object AtomsZZFiltering {

  case class _FilterUnsafe(pred: Obj => Boolean, max: Option[Int]) extends AtomZZ { def naive(z: Objs) =
      z.filter(pred).take(max) }

  // ---------------------------------------------------------------------------
  case class _FilterBy1[T](ori: PathPair, pred: T => Boolean, max: Option[Int]) extends AtomZZ { def naive(z: Objs) =
      z.filter(ori.lookup(_).asInstanceOf[T].pipe(pred)).take(max) }

    case class _FilterBy2[T1, T2](ori: PathPair2, pred: (T1, T2) => Boolean, max: Option[Int]) extends AtomZZ { def naive(z: Objs) =
      z.filter(ori.lookup(_).asInstanceOf[(T1, T2)].pipe(pred.tupled)).take(max) }

    case class _FilterBy3[T1, T2, T3](ori: PathPair3, pred: (T1, T2, T3) => Boolean, max: Option[Int]) extends AtomZZ { def naive(z: Objs) =
      z.filter(ori.lookup(_).asInstanceOf[(T1, T2, T3)].pipe(pred.tupled)).take(max) }
}

// ===========================================================================
