import scala.collection.mutable._

// ===========================================================================
/** to help with cross-build scala where sources are incompatibile */
package object cross { // 210214150322

  // TODO: any way to leverage scala.util.Properties.versionNumberString? combined with Class.forName maybe?
  type MutList[T] = ArrayDeque [T]; val  MutList = ArrayDeque ; @inline def mutList[T](x: MutList[T]): List[T] = x       .toList; type SeqView[T] = scala.collection.SeqView[T]                          // 213
//type MutList[T] = MutableList[T]; val  MutList = MutableList; @inline def mutList[T](x: MutList[T]): List[T] = x.result.toList; type SeqView[T] = scala.collection.SeqView[T, scala.collection.Seq[_]] // 212
}

// ===========================================================================
