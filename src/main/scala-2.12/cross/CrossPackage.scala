import scala.collection.mutable._

// ===========================================================================
package object cross { // for 2.12
          type MutList[T]                         = MutableList[T]
          val  MutList                            = MutableList
  @inline def  mutList[T](x: MutList[T]): List[T] = x.result.toList
  
  // ---------------------------------------------------------------------------
  type SeqView[T] = scala.collection.SeqView[T, scala.collection.Seq[_]]
}

// ===========================================================================
