package gallia
package heads

import scala.reflect.runtime.universe._

// ===========================================================================
package object sorting extends _heads {
  type SingleSelection = SEL.SortingSingle  .Selector
  type MultiSelection  = SEL.SortingMultiple.Selector

  // ---------------------------------------------------------------------------
  type SortingT[T]     = TSL.Sorting.TSelector[T]

  // ---------------------------------------------------------------------------
  // see https://stackoverflow.com/questions/18729321/how-to-get-classtag-form-typetag-or-both-at-same-time
  private[heads] def ctag[T: WeakTypeTag] = scala.reflect.ClassTag[T](
      weakTypeTag[T].mirror.runtimeClass(weakTypeTag[T].tpe))
}

// ===========================================================================