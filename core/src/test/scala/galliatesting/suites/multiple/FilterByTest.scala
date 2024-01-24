package galliatesting
package suites
package multiple

import gallia._

// ===========================================================================
object FilterByTest extends utest.TestSuite with GalliaTestSuite with TestDataO with TestDataS { import utest._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test(Default52.take    (1).check(bobjs(Default01)))
    test(Default52.takeLeft(1).check(bobjs(Default01)))

    test(Default52.drop    (1).check(bobjs(           Default01, Default01b)))
    test(Default52.dropLeft(1).check(bobjs(           Default01, Default01b)))

    test(Default52.takeWhile(_.string(f).mapV(_.size < 4)).check(bobjs(Default01, Default01)))
    test(Default52.dropWhile(_.string(f).mapV(_.size < 4)).check(bobjs(                      Default01b)))

    test(Default52.toViewBased    .take    (1).check(bobjs(Default01)))
    test(Default52.toIteratorBased.take    (1).display())
    test(Default52.toIteratorBased.take    (1).check(bobjs(Default01)))

    // ---------------------------------------------------------------------------
    test(Default52.filterBy(_.string(f))           .matches(_ == "foo2")              .force.one.check(Default01b))
  //test(Default52.filterBy(_.string(f))           .hasValue    ("foo2")              .force.one.check(Default01b))
    test(Default52.filterBy(         f )           .matches(_ == "foo2")              .force.one.check(Default01b))
    test(Default52.filterBy(         f )           .hasValue    ("foo2")              .force.one.check(Default01b))
    test(Default52.filterBy(         f )           .in(Seq("foo1", "foo2"))           .force.one.check(Default01b)) // like a SQL IN
    test(Default52.filterBy(_.string(f), _.int(g)).matches((f, g) => (f.size + g) > 4).force.one.check(Default01b))

    test(Default52.filterUnsafe { o => (o.string(f).size + o.int(g)) > 4 }            .force.one.check(Default01b))

    test(res0.filterBy(_.entity  (p)).matches(_.squash(_.string(f), _.int(g)).using(                (f, g) => (f.size + g) >  4  )).force.one.check(bobj(z -> false, p ->     Default01b )))
    test(res1.filterBy(_.entities(p)).matches(_.squash(_.string(f), _.int(g)).using(_.forall { case (f, g) => (f.size + g) >  4 })).force.one.check(bobj(z -> false, p -> Seq(Default01b))))
    test(Default52                 .takeWhile(_.squash(_.string(f), _.int(g)).using(                (f, g) => (f.size + g) <= 4  )).check(bobjs(Default01, Default01)))
    test(Default52                 .dropWhile(_.squash(_.string(f), _.int(g)).using(                (f, g) => (f.size + g) <= 4  )).check(bobjs(Default01b)))

    // ---------------------------------------------------------------------------
  //test(Default52.filterBy(_.string(f)).hasValue(foo).check(bobjs(Default01, Default01)))
    test(Default52.filterBy(         f ).hasValue(foo).check(bobjs(Default01, Default01)))
    test(Default52.findBy  (_.string(f)).hasValue(foo).check(bobjs(Default01)))

    test(Default52.filterBy(_.string(f)).matches(_ == foo).check(bobjs(Default01, Default01)))
    test(Default52.findBy  (_.string(f)).matches(_ == foo).check(bobjs(Default01)))

    test(Default52.filterBy(_.string(f)).matches(_ == FOO).checkEmpty())
    test(Default52.findBy  (_.string(f)).matches(_ == FOO).checkEmpty())

    // ---------------------------------------------------------------------------
    test(Default57.filterBy(_.string_(f)).matches(_.isDefined).check(aobjs(Default13p, Default13p2, Default13p)))
  //test(Default57.filterBy(_.any_(f))   .matches(_.isDefined)) // TODO: allow?
    test(Default57.filterBy(f).isPresent                      .check(aobjs(Default13p, Default13p2, Default13p)))
  //test(Default57.filterBy(f).hasSize(1)                     .check(aobjs(Default13p, Default13p2, Default13p)))
    test(Default57.filterBy(f).hasValue(     foo)             .check(aobjs(Default13p,              Default13p)))
    test(Default57.filterBy(f).matches (_ == foo)             .check(aobjs(Default13p,              Default13p)))
    test(Default59.filterBy(f).hasValue(Seq(foo1, foo2))      .check(aobjs(Default14p,              Default14p)))

    // ---------------------------------------------------------------------------
    test(Default52.filterBy(f, g).matches { (f, g) => f == foo && g == 1 }.check(bobjs(Default01,  Default01)))
    test(Default57.filterBy(f, g).matches { (f, g) => f == foo && g == 1 }.check(aobjs(Default13p, Default13p)))
    test(Default52.filterBy(f, g).hasValues(foo, 1).check(bobjs(Default01,  Default01)))
    test(Default57.filterBy(f, g).hasValues(foo, 1).check(aobjs(Default13p, Default13p)))

    test(Default52.filterBy(f, g).notValues(foo2, 2).check(bobjs(Default01,  Default01)))
    test(Default57.filterBy(f, g).notValues(foo2, 2).check(aobjs(Default13p, Default13p)))

    // ---------------------------------------------------------------------------
    test(Default57.filterBy(g).matches (_ == 1)           .check(aobjs(Default13p, Default13m, Default13p)))

    // ---------------------------------------------------------------------------
    test(aobjs(
          aobj(cls(f.int_, g.int))(obj(f -> 3, g -> 1)),
          aobj(cls(f.int_, g.int))(obj(f -> 3, g -> 2)),
          aobj(cls(f.int_, g.int))(obj(f -> 3, g -> 1)) )
        .pipe { expected =>
        Default57b.filterBy(f).matches (_ == 3).check(expected)
        Default57b.filterBy(f).matches (_ >= 3).check(expected)
      })

    // ===========================================================================
    test(bobjs(bobj(_line -> foo), bobj(_line -> ""), bobj(_line -> bar))
        .filterOutEmptyLines
      .check(bobjs(bobj(_line -> foo), bobj(_line -> bar))))
} }

// ===========================================================================