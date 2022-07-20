package gallia
package data
package multiple

import domain.SortingPair
import atoms.utils.SuperMetaPair.optionObjSuperMetaPair
import atoms.utils.SortWrapping.SortWrapperN
import atoms.utils.GalliaSpilling._

// ===========================================================================
trait ObjsSorting { self: Objs => // 220629103917 - by-pass mechanism for iterator

  // ---------------------------------------------------------------------------
  def sort[T](c: Cls, wrapper: SortWrapperN[T]): Objs =
    _modifyUnderlyingStreamer {
      if (!isIteratorBased) _.sortBy(wrapper.meta)(wrapper.data)
      else                  spillingSort(wrapper.pnfs(c), wrapper.gnuSortFields)(c) }

  // ---------------------------------------------------------------------------
  def sortByAll(c: Cls, pair: SortingPair): Objs =
    _modifyUnderlyingStreamer {
      if (!isIteratorBased) _.sortBy(optionObjSuperMetaPair(c, pair)) { _.retainOpt(c.keyz) }
      else                  spillingSortAll(c)(pair.descendingSpilling) }

  // ---------------------------------------------------------------------------
  def sortUnsafe[K](f: Obj => K, meta: atoms.utils.SuperMetaPair[K]): Objs = //(c: Cls, pair: SortingPair): Objs =
    _modifyUnderlyingStreamer {
      if (!isIteratorBased) _.sortBy(meta)(f)
      else                  ??? } //spillingSortUnsafe(c)(pair.descendingSpilling) }

  // ===========================================================================
  def distinct(c: Cls): Objs =
    _modifyUnderlyingStreamer {
      if (!isIteratorBased) _.distinct
      else                  spillingSortDistinct(c) }

}

// ===========================================================================