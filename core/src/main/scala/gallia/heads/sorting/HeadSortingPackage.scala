package gallia
package heads

// ===========================================================================
package object sorting extends _heads {
  type SingleSelection = SEL.SortingSingle  .Selector
  type MultiSelection  = SEL.SortingMultiple.Selector

  // ---------------------------------------------------------------------------
  type SortingT[T]     = TSL.Sorting.TSelector[T] }

// ===========================================================================