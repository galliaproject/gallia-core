import scala.collection.mutable._

// ===========================================================================
/** to help with cross-build scala where sources are incompatibile */
package object cross { // 210214150322
  type MutList[T] = ArrayDeque [T]; val  MutList = ArrayDeque ; @inline def mutList[T](x: MutList[T]): List[T] = x       .toList; type SeqView[T] = scala.collection.SeqView[T]                          // 213
}

// ===========================================================================
