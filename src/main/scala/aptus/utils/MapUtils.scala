package aptus.utils

import collection.{mutable, immutable}

// ===========================================================================
object MapUtils {

  def toMutableMap[A, T, U](coll: Seq[A])(implicit ev: A <:< (T, U)): mutable.HashMap[T, U] = {
    val b = mutable.HashMap.newBuilder[T, U]; for (x <- coll) b += x; b.result() }

  def toHashMap[A, T, U](coll: Seq[A])(implicit ev: A <:< (T, U)): immutable.HashMap[T, U] = {
    val b = immutable.HashMap.newBuilder[T, U]; for (x <- coll) b += x; b.result() }

  def toListMap[A, T, U](coll: Seq[A])(implicit ev: A <:< (T, U)): immutable.ListMap[T, U] = {
    val b = immutable.ListMap.newBuilder[T, U]; for (x <- coll) b += x; b.result() }

  def toTreeMap[A, T, U](coll: Seq[A])(implicit ev: A <:< (T, U), ord: Ordering[T]): immutable.TreeMap[T, U] = {
    val b = immutable.TreeMap.newBuilder[T, U]; for (x <- coll) b += x; b.result() }

  // ===========================================================================
  def toMutableMap[K, V](coll: Map[K, V]): mutable.HashMap[K, V] = {
    val b = mutable.HashMap.newBuilder[K, V]; for (x <- coll) b += x; b.result() }

  def toHashMap[K, V](coll: Map[K, V]): immutable.HashMap[K, V] = {
    val b = immutable.HashMap.newBuilder[K, V]; for (x <- coll) b += x; b.result() }

  def toListMap[K, V](coll: Map[K, V]): immutable.ListMap[K, V] = {
    val b = immutable.ListMap.newBuilder[K, V]; for (x <- coll) b += x; b.result() }

  def toTreeMap[K, V](coll: Map[K, V])(implicit ord: Ordering[K]): immutable.TreeMap[K, V] = {
    val b = immutable.TreeMap.newBuilder[K, V]; for (x <- coll) b += x; b.result() }

  // ===========================================================================
  // all these below are inspired by TraversableLike's (2.12)

  def groupByKey[K, V](entries: Iterator[(K, V)]): immutable.Map[K, Seq[V]] = {
      val m = mutable.Map.empty[K, cross.MutList[V]]

      for (elem <- entries) {
        val key = elem._1
        val bldr = m.getOrElseUpdate(key, cross.MutList[V]())
        bldr += elem._2
      }

      val b = immutable.Map.newBuilder[K, Seq[V]]
      for ((k, v) <- m)
        b += ((k, cross.mutList(v)))

      b.result
    }

    // ---------------------------------------------------------------------------
    def groupByKeyWithListMap[K, V](entries: Iterator[(K, V)]): immutable.ListMap[K, Seq[V]] = {
      var m = immutable.ListMap.empty[K, cross.MutList[V]]

      for (elem <- entries) {
        val key = elem._1
        val bldr =
          m.get(key) match { //m.getOrElseUpdate(key, cross.MutList[V]())
            case Some(x) => x
            case None =>
              val x = cross.MutList[V]()
              m = m + (key -> x) // -------------> seems like '+=' doesn't append? TODO: t210115142355 - investigate
              x }
        bldr += elem._2
      }

      val b = immutable.ListMap.newBuilder[K, Seq[V]]
      for ((k, v) <- m)
        b += ((k, cross.mutList(v)))

      b.result
    }

    // ---------------------------------------------------------------------------
    def groupByKeyWithTreeMap[K, V](entries: Iterator[(K, V)])(implicit ord: Ordering[K]): immutable.TreeMap[K, Seq[V]] = {
      val m = mutable.TreeMap.empty[K, cross.MutList[V]]

      for (elem <- entries) {
        val key = elem._1
        val bldr = m.getOrElseUpdate(key, cross.MutList[V]())
        bldr += elem._2
      }

      val b = immutable.TreeMap.newBuilder[K, Seq[V]]
      for ((k, v) <- m)
        b += ((k, cross.mutList(v)))

      b.result
    }
}

// ===========================================================================
