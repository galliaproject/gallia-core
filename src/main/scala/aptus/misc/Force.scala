package aptus

import scala.collection.immutable.{ListMap, TreeMap}

// ===========================================================================
private[aptus] final class Force[A] private[aptus] (private val coll: Seq[A]) {
  def one: A = { assert(coll.size == 1,  coll.size); coll.head }

  def distinct: Seq[A] = { val coll2 = coll.distinct; Predef.assert(coll.size == coll2.size, (coll.size, coll2.size)); coll2 }

  // ---------------------------------------------------------------------------
  //t201117175058 - TODO: add useful error messages...
  def tuple2  = { val iter = coll.iterator; val x = (iter.next(), iter.next()                                                                                                        ); assert(!iter.hasNext, coll); x }
  def tuple3  = { val iter = coll.iterator; val x = (iter.next(), iter.next(), iter.next()                                                                                           ); assert(!iter.hasNext, coll); x }
  def tuple4  = { val iter = coll.iterator; val x = (iter.next(), iter.next(), iter.next(), iter.next()                                                                              ); assert(!iter.hasNext, coll); x }
  def tuple5  = { val iter = coll.iterator; val x = (iter.next(), iter.next(), iter.next(), iter.next(), iter.next()                                                                 ); assert(!iter.hasNext, coll); x }
  def tuple6  = { val iter = coll.iterator; val x = (iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next()                                                    ); assert(!iter.hasNext, coll); x }
  def tuple7  = { val iter = coll.iterator; val x = (iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next()                                       ); assert(!iter.hasNext, coll); x }
  def tuple8  = { val iter = coll.iterator; val x = (iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next()                          ); assert(!iter.hasNext, coll); x }
  def tuple9  = { val iter = coll.iterator; val x = (iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next()             ); assert(!iter.hasNext, coll); x }
  def tuple10 = { val iter = coll.iterator; val x = (iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next()); assert(!iter.hasNext, coll); x }

  // ---------------------------------------------------------------------------
  // how often are we actually ok just silently discarding duplicates?
  def map[K, V]                             (implicit ev: A <:< (K, V)): Map[K, V] = { val map = aptus.utils.MapUtils.toHashMap(coll); assert(coll.size == map.size, (coll.size, map.size))  ; map }
  def map[K, V](debug: A => Any, anys: Any*)(implicit ev: A <:< (K, V)): Map[K, V] = { val map = aptus.utils.MapUtils.toHashMap(coll); assert(coll.size == map.size, (coll.map(debug), anys)); map }

  def listMap[K, V]                             (implicit ev: A <:< (K, V)): ListMap[K, V] = { val map = aptus.utils.MapUtils.toListMap(coll); assert(coll.size == map.size, (coll.size, map.size))  ; map }
  def listMap[K, V](debug: A => Any, anys: Any*)(implicit ev: A <:< (K, V)): ListMap[K, V] = { val map = aptus.utils.MapUtils.toListMap(coll); assert(coll.size == map.size, (coll.map(debug), anys)); map }

  def treeMap[K, V]                             (implicit ev: A <:< (K, V), ord: Ordering[K]): TreeMap[K, V] = { val map = aptus.utils.MapUtils.toTreeMap(coll); assert(coll.size == map.size, (coll.size, map.size))  ; map }
  def treeMap[K, V](debug: A => Any, anys: Any*)(implicit ev: A <:< (K, V), ord: Ordering[K]): TreeMap[K, V] = { val map = aptus.utils.MapUtils.toTreeMap(coll); assert(coll.size == map.size, (coll.map(debug), anys)); map }

  // ===========================================================================
  def mapLeft [K   ](f : A => K            ): Map[K, A] = new Force(coll.map { x => f (x) ->    x  }).map
  def mapRight[   V](            f : A => V): Map[A, V] = new Force(coll.map { x =>    x  -> f (x) }).map
  def mapBoth [K, V](fk: A => K, fv: A => V): Map[K, V] = new Force(coll.map { x => fk(x) -> fv(x) }).map
}

// ===========================================================================
