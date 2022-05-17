package gallia
package atoms

// ===========================================================================
object AtomsUUUnionTypes {

  case class _FuseToUnion(origin1: Key, origin2: Key, dest: Key) extends AtomUU { def naive(o: Obj) =
      (o.attemptKey(origin1), o.attemptKey(origin2)) match {
        case (None    , None    ) => ???
        case (Some(x1), None    ) => o.add(dest, x1).remove(origin1)
        case (None    , Some(x2)) => o.add(dest, x2).remove(origin2)
        case (Some(x1), Some(x2)) => ??? } }

    // ---------------------------------------------------------------------------
    case class _FissionFromUnion(origin: Key, dest1: Key, dest2: Key, p1: Any => Boolean, p2: Any => Boolean) extends AtomUU { def naive(o: Obj) = {
      val value = o.forceKey(origin)

           if (p1(value)) o.add(dest1, value).remove(origin)
      else if (p2(value)) o.add(dest2, value).remove(origin)
      else ??? // FIXME: 220516132010
    }}

}

// ===========================================================================