package gallia
package streamer

import aptus.CloseabledIterator
import scala.collection.parallel.CollectionConverters._ // for 2.13 only

// ===========================================================================
object IteratorParHack { // 210303141926; off by default
  val Cpus: Int = Runtime.getRuntime().availableProcessors() // TODO: logical threads or actual cores (former in my case)?    

  // ---------------------------------------------------------------------------
  @inline def     map[A, B](f: A =>      B )(itr: CloseabledIterator[A]): CloseabledIterator[B] = Hacks.iteratorParGroupSize.map(    mapPar(itr)(f)).getOrElse(itr.map(f))
  @inline def flatMap[A, B](f: A => Coll[B])(itr: CloseabledIterator[A]): CloseabledIterator[B] = Hacks.iteratorParGroupSize.map(flatMapPar(itr)(f)).getOrElse(itr.flatMap(f))
  
    // ---------------------------------------------------------------------------
    @inline def     mapPar[A, B](itr: CloseabledIterator[A])(f: A =>      B )(groupSize: Int): CloseabledIterator[B] = itr.alter(_.grouped(Cpus * groupSize)).flatMap(_.par.    map(f))
    @inline def flatMapPar[A, B](itr: CloseabledIterator[A])(f: A => Coll[B])(groupSize: Int): CloseabledIterator[B] = itr.alter(_.grouped(Cpus * groupSize)).flatMap(_.par.flatMap(f))
}

// ===========================================================================
