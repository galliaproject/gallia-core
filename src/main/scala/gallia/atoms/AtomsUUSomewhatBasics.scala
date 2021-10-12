package gallia
package atoms

// ===========================================================================
object AtomsUUSomewhatBasics {

  // ---------------------------------------------------------------------------
  case class _RemoveIf(key: Key, pred: AnyValue => Boolean) extends AtomUU { def naive(o: Obj) =
    o .opt(key)
      .map { value =>
        if (pred(value)) o.remove(key)
        else             o }
      .getOrElse(o) }

  // ---------------------------------------------------------------------------
  case class _RemoveWhateverIf(key: Key, value: AnyValue) extends AtomUU { def naive(o: Obj) =
    o .opt(key)
      .map { _value =>
        if (_value == value) o.remove(key)
        else                 o }
      .getOrElse(o) }

  // ---------------------------------------------------------------------------
  case class _RemoveWhateverIfAll(map: Map[Key, AnyValue]) extends AtomUU with AtomCombiner[_RemoveWhateverIf] {
      def naive(o: Obj) =
        o ._data
          .flatMap { case (key, value) =>
            map.get(key) match {
              case None              =>                                     Some(key -> value)
              case Some(targetValue) => if (value == targetValue) None else Some(key -> value) } }
          .pipe(Obj.build0)
    }
  
    // ---------------------------------------------------------------------------
    object _RemoveWhateverIfAll {

      def from(values: Seq[_RemoveWhateverIf]): _RemoveWhateverIfAll =
        values
          .map { x => x.key -> x.value }
          .toMap
          .pipe(_RemoveWhateverIfAll.apply)

    }

  // ===========================================================================
  case class _SetDefault(key: Key, value: AnyValue) extends AtomUU { def naive(o: Obj) =
    if (o.contains(key)) o else o.add(key, value) }

  // ===========================================================================
  case class _Split(key: Key, splitter: StringSplitter) extends AtomUU { def naive(o: Obj) =
    o.transformString(key, splitter.apply) }

  // ---------------------------------------------------------------------------
  case class _Swap(key1: Key, key2: Key) extends AtomUU { def naive(o: Obj) =
    o.swapEntries(key1, key2) }

  // ---------------------------------------------------------------------------
  case class _Copy(key1: Key, key2: Key) extends AtomUU { def naive(o: Obj) =
    o.copyEntries(key1, key2) }

  // ===========================================================================
  case class _Nest(target: Keyz, nestingKey: Key) extends AtomUU { def naive(o: Obj) =
      o.nest(target, nestingKey) }

  // ===========================================================================
  case class _UnnestOOO(parent: KPath, key: Key) extends AtomUU { def naive(o: Obj) =
      utils.AtomsHelper.unnestOOO(o, parent, key) }

    // ---------------------------------------------------------------------------
    case class _UnnestAll(parent: KPath) extends AtomUU { def naive(o: Obj) = // TODO: still worth handling separately?
      utils.AtomsHelper.unnestAll(o, parent) }

    // ---------------------------------------------------------------------------
    case class _UnnestSome(parent: KPath, keyz: Keyz) extends AtomUU { def naive(o: Obj) =
      utils.AtomsHelper.unnestSome(o, parent, keyz) }

  // ===========================================================================
  @deprecated("WIP: t210109144926") case class _Move(target: KPath, destinationOpt: Option[KPath]) extends AtomUU { def naive(o: Obj) = ??? } //_helper.move(o, target, destinationOpt) }

    @deprecated("WIP: t210109144926") case class _Move2(targets: KPathz, destinationOpt: Option[KPath]) extends AtomUU { def naive(o: Obj) = ??? }//_helper.move2() }

}

// ===========================================================================

