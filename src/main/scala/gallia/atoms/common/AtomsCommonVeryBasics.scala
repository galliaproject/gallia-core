package gallia
package atoms
package common

import aptus.Anything_
import data.single.RetainMapping

// ===========================================================================
object AtomsCommonVeryBasics {

  // ideally would these three be purely meta operations... (TODO: t210104164036)
  case class _ReorderKeys(f: Seq[Key] => Seq[Key]) extends AtomUU { def naive(o: Obj) =
        o.keys.pipe(f).map(_.associateRight(o.forceKey(_))).pipe(gallia.obj) }

    // ---------------------------------------------------------------------------
    case class _ReorderKeysRecursively(f: Seq[Key] => Seq[Key]) extends AtomUU { def naive(o: Obj): Obj =
        utils.AtomsHelper.reorderKeysRecursively(f)(o) }

    // ---------------------------------------------------------------------------
    case class _Rename(value: ActualRen) extends AtomUU { def naive(o: Obj) =
      o.rename(value) }

    // ---------------------------------------------------------------------------
    case class _RenameAll(mapping: Map[Key, Key]) extends AtomUU with AtomCombiner[_Rename] {
        def naive(o: Obj) =  
            o ._data
              .map { case (key, value) => mapping.getOrElse(key, key) -> value }
              .pipe(Obj.build) }

      // ---------------------------------------------------------------------------
      object _RenameAll {

        def from(values: Seq[_Rename]): _RenameAll =
          values
            .map(_.value.pair)
            .toMap
            .pipe(_RenameAll.apply)            
      
      }

  // ===========================================================================
  case class _Remove(key: Key) extends AtomUU { def naive(o: Obj) =
    o.remove(key) }

  // ---------------------------------------------------------------------------
  case class _Retain(paths: KPathz, mapping: RetainMapping) extends AtomUU { def naive(o: Obj) =
    o.retain(paths, mapping) }

  // ---------------------------------------------------------------------------
  case class _Add(key: Key, value: AnyValue) extends AtomUU { def naive(o: Obj) =
    o.putKey(key, value) } //TODO: serialization issues?...

}

// ===========================================================================

