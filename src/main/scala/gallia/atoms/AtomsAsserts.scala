package gallia.atoms

import aptus.{Anything_, Seq_}
import gallia._
import gallia.domain.PathPair

// ===========================================================================
object AtomsAsserts {

  case class _AssertUnsafeO(pred: Obj => Boolean) extends AtomUU { def naive(o: Obj) =
        _Error.Runtime.DataUnsafeUAssertionFailure.runtimeIfU(o)(!pred(_)) }

      // ---------------------------------------------------------------------------
      case class _AssertUnsafeZ(pred: Objs => Boolean) extends AtomZZ { def naive(z: Objs) =
        _Error.Runtime.DataUnsafeZAssertionFailure.runtimeIfZ(z)(!pred(_)) }

  // ===========================================================================
  case class _AssertIsDefined(paths: KPathz) extends AtomUU { def naive(o: Obj) = {
      val undefined: Seq[KPath] = paths.values.filter(!o.contains(_)) // FIXME: t210110203020 - contains needs to be able to deal with multiplicity in nesting(s)

      if (undefined.nonEmpty) _Error.Runtime.NotDefined(KPathz(undefined)).throwDataError(o)
      else o
    }
  }

  // ===========================================================================
  case class _ForceOneA(ori: Key) extends AtomUU { def naive(o: Obj) = o.force(ori)    .asInstanceOf[Seq[_]].force.one.pipe { v => o.put(ori, v) } }
  case class _ForceOneB(ori: Key) extends AtomUU { def naive(o: Obj) = o.opt(ori).map(_.asInstanceOf[Seq[_]].force.one.pipe { v => o.put(ori, v) }).getOrElse(o) }

  // ---------------------------------------------------------------------------
  case class _ForceSeqA(ori: Key) extends AtomUU { def naive(o: Obj) = o.force(ori)    .in.list.pipe { v => o.put(ori, v) } }
  case class _ForceSeqB(ori: Key) extends AtomUU { def naive(o: Obj) = o.opt(ori).map(_.in.list.pipe { v => o.put(ori, v) }).getOrElse(o) }

  // ===========================================================================
  @Max5
  case class _AssertO1a(ori: KPath, pred: Any => Boolean) extends AtomUU { def naive(o: Obj) =
      _Error.Runtime.DataAssertionFailure(ori).attemptO(o) {
        !_.force(ori).pipe(pred) } }

    case class _AssertO1b(ori: KPath, pred: Option[Any] => Boolean) extends AtomUU { def naive(o: Obj) =
      _Error.Runtime.DataAssertionFailure(ori).attemptO(o) {
        !_.opt(ori).pipe(pred) } }

  // ---------------------------------------------------------------------------
  case class _AssertO2(squasher: AtomsUV._SquashU2) extends AtomUU { def naive(o: Obj) =
    _Error.Runtime.DataAssertionFailure(squasher.ori).attemptO(o) {
        !squasher.naive(_).asInstanceOf[Boolean] /* by design */ } }

  // ---------------------------------------------------------------------------
  case class _AssertO3(squasher: AtomsUV._SquashU3) extends AtomUU { def naive(o: Obj) =
    _Error.Runtime.DataAssertionFailure(squasher.ori).attemptO(o){
        !squasher.naive(_).asInstanceOf[Boolean] /* by design */ } }

  // ===========================================================================
  case class _AssertSameType(from: PathPair, to: KPath) extends AtomUU { def naive(o: Obj) = {
    	gallia.data.ValueUtils.checkSameTypes(
    	    from = from                .lookup(o),
    	    to   = from.copy(path = to).lookup(o))

      o
    }
  }

}

// ===========================================================================
