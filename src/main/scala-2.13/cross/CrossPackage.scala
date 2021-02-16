import scala.collection.mutable._

// ===========================================================================
package object cross { // for 2.13
          type MutList[T]                         = ArrayDeque[T]
          val  MutList                            = ArrayDeque
  @inline def  mutList[T](x: MutList[T]): List[T] = x.toList // no more ".result" (MutableList)
  
  // ---------------------------------------------------------------------------
  type SeqView[T] = scala.collection.SeqView[T] // no more second type parameter: [T, Seq[_]]
}

// ===========================================================================
