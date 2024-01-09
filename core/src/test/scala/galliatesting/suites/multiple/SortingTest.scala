package galliatesting
package suites
package multiple

import gallia._

// ===========================================================================
object SortingTest extends utest.TestSuite with GalliaTestSuite with TestDataS { import utest._
  import TestDataO._
  import gallia.vldt._Error

  // ---------------------------------------------------------------------------
  private[SortingTest] implicit class BObjs___(u: BObjs) { def mode(asIterator: Boolean): HeadS = if (asIterator) u.toIteratorBased else u }
  private[SortingTest] implicit class AObjs___(u: AObjs) { def mode(asIterator: Boolean): HeadS = if (asIterator) u.toIteratorBased else u }

  // ===========================================================================
  val tests = Tests {

    // TODO: t231228133031 - how to refactor code with utest? keeping as asserts until then
    def _test(itr: Boolean /* asIterator */): Unit = { val mem = !itr
      Default52.mode(itr).distinct.check(Default51)

      // ---------------------------------------------------------------------------
      // ensure uniqueness
      Default52.mode(itr).ensureUniqueness        .dataError[_Error.Runtime.NotUnique]
      Default52.mode(itr).ensureUniquenessBy(f   ).dataError[_Error.Runtime.NotUnique]
      Default52.mode(itr).ensureUniquenessBy(f, g).dataError[_Error.Runtime.NotUnique]

      Default51.mode(itr)._forceResult.noop(_.ensureUniqueness)
      Default51.mode(itr)._forceResult.noop(_.ensureUniquenessBy(f))
      Default51.mode(itr)._forceResult.noop(_.ensureUniquenessBy(f, g))

      // ===========================================================================
      // descending

      Default52  .mode(itr).sortAscending           .check(bobjs(Default01, Default01, Default01b))

      Default52  .mode(itr).sortByDescendingFirstKey.check(bobjs(Default01b, Default01, Default01))
      Default52  .mode(itr).sortDescending          .check(bobjs(Default01b, Default01, Default01))

      Default52ef.mode(itr).sortByDescendingFirstKey.check(bobjs(Default01f, Default01e, Default01e, Default01g))
      Default52ef.mode(itr).sortDescending          .check(bobjs(Default01f, Default01g, Default01e, Default01e)) // only differing

      Default56  .mode(itr).sortByDescendingFirstKey.check(bobjs(Default06b, Default06a, Default06a))
      Default56  .mode(itr).sortDescending          .check(bobjs(Default06b, Default06a, Default06a))

      Default57  .mode(itr).sortByDescendingFirstKey.check(aobjs(            Default13p, Default13p, Default13p2, Default13m))
      Default57  .mode(itr).sortDescending          .check(
        // see 220708155855
        if (mem) aobjs(            Default13p, Default13p, Default13p2, Default13m)
        else     aobjs(Default13m, Default13p, Default13p, Default13p2))

      Default59  .mode(itr).sortByDescendingFirstKey.check(aobjs(Default14p, Default14p, Default14p2, Default14m))
      Default59  .mode(itr).sortDescending          .check(
        // see 220708155855
        if (mem) aobjs(            Default14p, Default14p, Default14p2, Default14m)
        else     aobjs(Default14m, Default14p, Default14p, Default14p2))

      {
        val Default02z = bobj(f -> Seq("foo3", "foo4"), g -> 3)
        val Default60  = bobjs(Default02, Default02z)

        Default60.mode(itr).sortByDescendingFirstKey.check(bobjs(Default02z, Default02))
        Default60.mode(itr).sortDescending          .check(bobjs(Default02z, Default02))
      }

      // ===========================================================================
      // missing last (if-mem: see 220624173138)

      if (mem) Default57.mode(itr).sortBy(f, descending = false, missingLast = true ).check(aobjs(Default13p2, Default13p, Default13p, Default13m))
               Default57.mode(itr).sortBy(f, descending = true , missingLast = true ).check(aobjs(Default13p, Default13p, Default13p2, Default13m))
               Default57.mode(itr).sortBy(f, descending = false, missingLast = false).check(aobjs(Default13m, Default13p2, Default13p, Default13p))
      if (mem) Default57.mode(itr).sortBy(f, descending = true , missingLast = false).check(aobjs(Default13m, Default13p, Default13p, Default13p2))

      if (mem) {
        Default57.mode(itr).sort  (descending = false, missingLast = true ).check(aobjs(Default13p2, Default13p, Default13p, Default13m))
        Default57.mode(itr).sort  (descending = true , missingLast = true ).check(aobjs(Default13p, Default13p, Default13p2, Default13m))
        Default57.mode(itr).sort  (descending = false, missingLast = false).check(aobjs(Default13m, Default13p2, Default13p, Default13p))
        Default57.mode(itr).sort  (descending = true , missingLast = false).check(aobjs(Default13m, Default13p, Default13p, Default13p2)) }

      if (mem) Default59.mode(itr).sortBy(f, descending = false, missingLast = true ).check(aobjs(Default14p2, Default14p, Default14p, Default14m))
               Default59.mode(itr).sortBy(f, descending = true , missingLast = true ).check(aobjs(Default14p, Default14p, Default14p2, Default14m))
               Default59.mode(itr).sortBy(f, descending = false, missingLast = false).check(aobjs(Default14m, Default14p2, Default14p, Default14p))
      if (mem) Default59.mode(itr).sortBy(f, descending = true , missingLast = false).check(aobjs(Default14m, Default14p, Default14p, Default14p2))

      if (mem) {
        Default59.mode(itr).sort  (descending = false, missingLast = true ).check(aobjs(Default14p2, Default14p, Default14p, Default14m))
        Default59.mode(itr).sort  (descending = true , missingLast = true ).check(aobjs(Default14p, Default14p, Default14p2, Default14m))
        Default59.mode(itr).sort  (descending = false, missingLast = false).check(aobjs(Default14m, Default14p2, Default14p, Default14p))
        Default59.mode(itr).sort  (descending = true , missingLast = false).check(aobjs(Default14m, Default14p, Default14p, Default14p2)) }

      // ===========================================================================
      // multiple

      Default56.mode(itr).sortBy   (_.firstKey) .check(bobjs(Default06a, Default06a, Default06b))
      Default56.mode(itr).sortByAll(_.initKeys) .check(bobjs(Default06a, Default06a, Default06b))
      Default56.mode(itr).sortByAll(Seq(f1, f2)).check(bobjs(Default06a, Default06a, Default06b))

      // ===========================================================================
      val tmp: HeadZ = HeadZ.Dummy

        tmp.sortUsing(_.int("weight"), _.string("target")).using((w, t) => (-w, t))

        tmp.sortUsingUnsafe { o => (-o.int("weight"), o.string("target")) }

        tmp.sortUsingUnsafe { (x, y) =>
            val a = - x.int("weight")
            val b = - y.int("weight")

            val c = x.string("target")
            val d = y.string("target")

            implicitly[Ordering[(Int, String)]]
              .compare((a, c), (b, d)) }

        tmp.sortBy("weight".desc, "target")

      // ===========================================================================
      val o1 = bobj(f -> 1, g -> "a")
      val o2 = bobj(f -> 2, g -> "b")
      val o1212 = bobjs(o1, o2, o1, o2)

      // ---------------------------------------------------------------------------
      if (mem) {
        o1212.mode(itr).sortUsingUnsafe( _.int(f)).check(bobjs(o1, o1, o2, o2))
        o1212.mode(itr).sortUsingUnsafe(-_.int(f)).check(bobjs(o2, o2, o1, o1)) }

      o1212.mode(itr).sortBy    (f)         .check(bobjs(o1, o1, o2, o2))
      o1212.mode(itr).sortBy    (_.firstKey).check(bobjs(o1, o1, o2, o2))

      if (mem) {
        o1212.mode(itr).sortUsing(_.int(f))             .using(-_)               .check(bobjs(o2, o2, o1, o1))
        o1212.mode(itr).sortUsing(_.int(f), _.string(g)).using((f, g) => (g, -f)).check(bobjs(o1, o1, o2, o2)) }

      o1212.mode(itr).sort.check(bobjs(o1, o1, o2, o2))

      o1212.mode(itr).sortBy(f         , g)        .check(bobjs(o1, o1, o2, o2))
      o1212.mode(itr).sortBy(_.firstKey, _.lastKey).check(bobjs(o1, o1, o2, o2))

      if (mem) {
        o1212.mode(itr).sortBy(f.desc)   .check(bobjs(o2, o2, o1, o1))
        o1212.mode(itr).sortBy(f.desc, g).check(bobjs(o2, o2, o1, o1)) }

      // ---------------------------------------------------------------------------
      if (mem) o1212.mode(itr).sortBy(f.useDescending).check(bobjs(o2, o2, o1, o1)) // .sortReverseNumericallyBy

      if (false) {
        //@deprecated def sort(conf: Conf.Start => Conf.End): Self = ???
        //    o1212.sort(_.by(f    )).check(bobjs(o2, o2, o1, o1))   //_.by(f, _.numerical)
        //    o1212.sort(_.by(f, g)).check(bobjs(o2, o2, o1, o1))   //_.by(f, _.numerical, g, _.alphabetical)
        //    o1212.sort(_.by(f, g)).check(bobjs(o2, o2, o1, o1)) } //_.by(f             , g, _.alphabetical)
      }
    }

    // ---------------------------------------------------------------------------
    _test(itr = false)
    _test(itr = true) }
}

// ===========================================================================