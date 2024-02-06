package gallia
package oswo

// ===========================================================================
object OswoAtomMatcher {

  /** adds to finalCode */
  def apply(atom: Atom)(id: ANI, fromIdOpt: Option[ANI])(last: IsLast)(efferent: Cls /* just for IX... */): Unit =
    atom match {

      // ---------------------------------------------------------------------------
      case atomiu: AtomIU =>
        assert(fromIdOpt.isEmpty)
        val ctx = OswoCtx(None, id, last)

        finalCode += (atomiu match {
          case y: _JsonObjectString => OswoAtomFormatter.jsonObjectString(y)(efferent)(ctx)
          case y: _InMemoryInputUb  => OswoAtomFormatter.inMemoryInputUb (y)(efferent)(ctx) })

      // ===========================================================================
      case atomuu: AtomUU =>
        val ctx = OswoCtx(fromIdOpt, id, last)

        finalCode +=
          (atomuu match {
            case y: _TransformVV => OswoAtomFormatter.transformVV(y)(ctx).tap { _ => serials += id -> OswoSerDes.serializeFunction(y.f) }

            // ---------------------------------------------------------------------------
            case y: _Rename => OswoAtomFormatter.rename(y)(ctx)

            // ---------------------------------------------------------------------------
            case y: _ConvertToDouble => OswoAtomFormatter.convertToDouble(y)(ctx)

            // ---------------------------------------------------------------------------
            case y: _Add     => OswoAtomFormatter.add    (y)(ctx)
          //case y: _Replace => OswoAtomFormatter.replace(y)(ctx)
            case y: _Remove  => OswoAtomFormatter.remove (y)(ctx)

            // ===========================================================================
            case _ => //???
/* do nothing */ })

        // ---------------------------------------------------------------------------
        case _ => //???
/* do nothing */ }

}

// ===========================================================================
