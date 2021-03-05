package gallia.data.multiple.streamer

import scala.collection.parallel.CollectionConverters._
import gallia._

// ===========================================================================
object IteratorParHack { // 210303141926; off by default
  val Cpus: Int = Runtime.getRuntime().availableProcessors() // TODO: logical threads or actual cores (former in my case)?    

  // ---------------------------------------------------------------------------
  @inline def     map[A, B](itr: Iterator[A])(f: A =>      B ): Iterator[B] = Hacks.IteratorParGroupSize.map(    mapPar(itr)(f)).getOrElse(itr.map(f))
  @inline def flatMap[A, B](itr: Iterator[A])(f: A => Coll[B]): Iterator[B] = Hacks.IteratorParGroupSize.map(flatMapPar(itr)(f)).getOrElse(itr.flatMap(f))
  
    // ---------------------------------------------------------------------------
    @inline def     mapPar[A, B](itr: Iterator[A])(f: A =>      B )(groupSize: Int): Iterator[B] = itr.grouped(Cpus * groupSize).flatMap(_.par.    map(f))
    @inline def flatMapPar[A, B](itr: Iterator[A])(f: A => Coll[B])(groupSize: Int): Iterator[B] = itr.grouped(Cpus * groupSize).flatMap(_.par.flatMap(f))
}

// ===========================================================================
