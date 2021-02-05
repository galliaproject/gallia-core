package gallia.atoms

import gallia._

// ===========================================================================
object AtomsCustom {
  // also used by for-key

  case class _CustomOO(f: Obj  => Obj ) extends AtomUU { def naive(o: Obj ) = f(o) }
  case class _CustomZZ(f: Objs => Objs) extends AtomZZ { def naive(z: Objs) = f(z) }

  case class _CustomOZ(f: Obj  => Objs) extends AtomUZ { def naive(o: Obj ) = f(o) }
  case class _CustomZO(f: Objs => Obj ) extends AtomZU { def naive(o: Objs) = f(o) }

  case class _CustomOV(f: Obj  => Vle ) extends AtomUV { def naive(o: Obj ) = f(o) }
  case class _CustomZV(f: Objs => Vle ) extends AtomZV { def naive(z: Objs) = f(z) }

}

// ===========================================================================
