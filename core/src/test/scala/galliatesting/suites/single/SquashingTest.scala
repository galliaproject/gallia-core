package galliatesting
package suites
package single

// ===========================================================================
object SquashingTest extends utest.TestSuite with GalliaTestSuite with TestDataO with TestDataS { import utest._
  import gallia._
  import gallia.vldt.ErrorId._

  val tests = Tests {
    test(Default06a.fuse  (_.string(f1), _.int(g)).as(_tmp).using((f1, g) => f1.size + g).int(_tmp).check(5)) // equivalent but longer (220620111230)
    test(Default06a.squash(_.string(f1), _.int(g))         .using((f1, g) => f1.size + g)          .check(5))
    test(Default06a.squashUnsafe(o => o.string(f1).size + o.int(g))                                .check(5))

    test(Default03.transform(_.entity(p)).using(_.squash(_.string(f), _.int(g)).using((f, g) => f.size + g)            ).check(bobj(p -> 4, z -> true)))
    test(Default03.transform(_.entity(p)).using(_.squash(_.string(f), _.int(g)).using((f, g) => f.size + g).mapV(_ + 1)).check(bobj(p -> 5, z -> true)))
    test(Default03.transform(_.entity(p)).using(_.squashUnsafe(o => o.string(f).size + o.int(g)           )            ).check(bobj(p -> 4, z -> true)))
    test(Default03.transform(_.entity(p)).using(_.squashUnsafe(o => o.string(f).size + o.int(g)           ).mapV(_ + 1)).check(bobj(p -> 5, z -> true)))

    test(liftValue(3  ).mapV(_ + 1) .check(4))
    test(liftValue(foo).mapV(_.size).check(3))
    test(liftValue(foo).mapV(_.size).mapV(_ + 1).check(4))

    test(liftValue(Seq(1, 2, 3)).mapV     (_.map(_ + 1)).check(Seq(2, 3, 4)))
    test(liftValue(Seq(1, 2, 3)).mapVs[Int, Int](_ + 1) .check(Seq(2, 3, 4))) // worth keeping?
    test(liftValue(Seq(1, 2, 3)).mapV (_.reduceLeft(_ + _)).check(6))

    test({
      val (v1, v2) = (liftValue(foo), liftValue(1))

      v1.combine(v2).using((x, y) => x + (y + 1)).check(foo2) })

    // ===========================================================================
    test(Predef.assert(4 == Default01.forceSquashUnsafe(o => o.string(f).size + o.int(g))))
    test(Predef.assert(1 == Default01.forceInt         (g)))

    //u.transform(f).using(_.size); u.fuse

  // ---------------------------------------------------------------------------
  //test(Default01.squash(_.string(f)).using(identity).check(foo))
    test(Default01 .grab(_.string (f))                .check(foo))
    test(Default15p.grab(_.string_(f)).check(Some(foo)))
    test(Default15m.grab(_.string_(f)).check(None))

    // ---------------------------------------------------------------------------
    test("grab/squash") {
      test(Default01.squashUnsafe(o => o.string(f).size + o.int(g)).check(4))

      test(Default01.squash(_.int(g)).using(identity).check(1))
      test(Default01.grab  (_.int(g))                .check(1))
      test(Default01.forceInt    (g).ensuring(_ == 1))

      test(Default52.squashUnsafe  (_.map(_.int(g)).sum).check(4))

      test(Default52.squash(_.int(g)).using(_.sum).check(4))
      test(Default56.squash(_.string(f1), _.string(f2)).using { _.map { case (f1, f2) => f1.size * f2.size }.sum }.check(48))

      test(Default52.squash(_.int(g)).using(identity).check(Seq(1, 1, 2)))
      test(Default52.grab(_.int(g))                  .check(Seq(1, 1, 2)))
      if (false) /*FIXME*/ Default52.grab(_.ints(g))                  .check(Seq(Seq(1) /*, ...*/))
      test(Default52.forceInts(g).ensuring(_ == Seq(1, 1, 2)))
      test(Default52.ints     (g).check(        Seq(1, 1, 2)))

      test(
        aobjs(
            aobj(
              cls(f.string, g.int_))(
              obj(f-> foo1, g-> 1)),
            aobj(
              cls(f.string, g.int_))(
              obj(f-> foo2)) )
          .ints_(g).check(Some(Seq(1)))) }

    // ===========================================================================
//TODO: int vs ints inconsistency - also vs ooo
    val GrabIn  = bobj(f-> Seq(bobj(h-> 1), bobj(h-> 2)))
    val GrabOut = bobj(f-> Seq(1, 2))

    test(GrabIn.transform(_.entities(f)).using(_.grab(_.int (h))).check(GrabOut))
    test(GrabIn.transform(_.entities(f)).using(_       .ints(h) ).check(GrabOut))
    if(false) GrabIn.transform(_.entities(f)).using(_.grab  (h)) .check(GrabOut) // FIXME?
  }

  // ---------------------------------------------------------------------------
  private def liftValue[T: __WTT_for_testing_only](a: T): HeadV[T] = heads.Head.inputV(a)

}

// ===========================================================================
