package gallia
package atoms

// ===========================================================================
object AtomsCustom {
  // also used by for-key

  case class _CustomOO(f: Obj  => Obj ) extends AtomUU { def naive(o: Obj ) = f(o) }
  case class _CustomZZ(f: Objs => Objs) extends AtomZZ { def naive(z: Objs) = f(z) }

  case class _CustomOZ(f: Obj  => Objs) extends AtomUZ { def naive(o: Obj ) = f(o) }
  case class _CustomZO(f: Objs => Obj ) extends AtomZU { def naive(o: Objs) = f(o) }

  case class _CustomOV(f: Obj  => Vle ) extends AtomUV { def naive(o: Obj ) = f(o) }
  case class _CustomZV(f: Objs => Vle ) extends AtomZV { def naive(z: Objs) = f(z) }

  // ===========================================================================
  case class _CustomConditionalOO(p: Obj => Boolean, f: Obj => Obj) extends AtomUU { def naive(o: Obj) = if (!p(o)) o else f(o) }
}

// ===========================================================================
