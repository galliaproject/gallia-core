package gallia
package atoms

import aptus.Anything_

import domain._
import FunctionWrappers._

// ===========================================================================
object AtomsUV {

  @Distributivity case object _Size extends AtomZV { def naive(z: Objs) =
    z.size }

  // ===========================================================================
  case class _GrabU(ori: PathPair1) extends AtomUV { def naive(o: Obj) =
    ori.lookup(o) }

  // ---------------------------------------------------------------------------
  case class _GrabZOne(ori: KPath) extends AtomZV { def naive(z: Objs) =
      z.mapToStreamer(_.force(ori)).toList }

    case class _GrabZOpt(ori: KPath) extends AtomZV { def naive(z: Objs) =
      z.flatMapToStreamer(_.opt(ori)).toList.in.noneIf(_.isEmpty) }

  // ===========================================================================
  case class _SquashU1(ori: PathPair1, f: _ff11) extends AtomUV { def naive(o: Obj) =
        ori.lookup(o).pipe(f) }

    case class _SquashU2(ori: PathPair2, f: _ff21) extends AtomUV { def naive(o: Obj) =
      ori.lookup(o).pipe(f.tupled) }

    case class _SquashU3(ori: PathPair3, f: _ff31) extends AtomUV { def naive(o: Obj) =
      ori.lookup(o).pipe(f.tupled) }

    case class _SquashU4(ori: PathPair4, f: _ff41) extends AtomUV { def naive(o: Obj) =
      ori.lookup(o).pipe(f.tupled) }

    case class _SquashU5(ori: PathPair5, f: _ff51) extends AtomUV { def naive(o: Obj) =
      ori.lookup(o).pipe(f.tupled) }

  // ===========================================================================
  case class _SquashZ1(ori: PathPair1, f: _agg1) extends AtomZV { def naive(z: Objs) =
      z.mapToStreamer(ori.lookup).toList.pipe(f) }

    case class _SquashZ2(ori: PathPair2, f: _agg2) extends AtomZV { def naive(z: Objs) =
      z.mapToStreamer(ori.lookup).toList.pipe(f) }

    case class _SquashZ3(ori: PathPair3, f: _agg3) extends AtomZV { def naive(z: Objs) =
      z.mapToStreamer(ori.lookup).toList.pipe(f) }

    case class _SquashZ4(ori: PathPair4, f: _agg4) extends AtomZV { def naive(z: Objs) =
      z.mapToStreamer(ori.lookup).toList.pipe(f) }

    case class _SquashZ5(ori: PathPair5, f: _agg5) extends AtomZV { def naive(z: Objs) =
      z.mapToStreamer(ori.lookup).toList.pipe(f) }
}

// ===========================================================================
