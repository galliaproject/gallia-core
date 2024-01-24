package galliatesting
package suites
package multiple

import gallia._

// ===========================================================================
object GroupingTest extends utest.TestSuite with GalliaTestSuite with TestDataO with TestDataS { import utest._
  private val Input = bobjs(Default03, bobj(p -> Default01b, z -> false), Default03)

  // ---------------------------------------------------------------------------
  private val Output1 = bobjs(bobj(f -> "foo", g -> Seq(1, 1)), bobj(f -> "foo2", g -> Seq(2) ))
  private val Output2 = bobjs(bobj(f -> "foo", _group -> Seq(bobj(g -> 1), bobj(g -> 1))), bobj(f -> "foo2", _group -> Seq(bobj(g -> 2)) ))
  private val Output3 = bobjs(bobj(f -> "foo", "GROUP" -> Seq(bobj(g -> 1), bobj(g -> 1))), bobj(f -> "foo2", "GROUP" -> Seq(bobj(g -> 2)) ))
  private val Output4 = bobjs(bobj(f -> Seq("foo1", "foo2"), g -> Seq(1, 1)), bobj(f -> Seq("foo3", "foo4"), g -> Seq(3)))

  private val Output5 = aobjs(cls(f.string_, g.ints))(
    obj(f -> "foo", g -> Seq(1, 1)),
    obj(             g -> Seq(1)),
    obj(f -> "bar", g -> Seq(2)))

  private val Output6 = aobjs(cls(f.strings_, g.ints))(
    obj(f -> Seq("foo1", "foo2"), g -> Seq(1, 1)),
    obj(                           g -> Seq(1)),
    obj(f -> Seq("bar1", "bar2"), g -> Seq(2)))

  // ===========================================================================
  val tests = Tests {
    test( { // TODO: 240109132514 - how to refactor code with utest?
        _test(inMemory = true)
        _test(inMemory = false) }) }

    // ---------------------------------------------------------------------------
    private def _test(inMemory: Boolean): Unit = { implicit def idef220705141138(values: BObjs): AObjs = values.forceAObjs

      def input(values: AObjs) = if (inMemory) values.toViewBased else values.toIteratorBased

      // ---------------------------------------------------------------------------
      // because of 220705134404 (group-by comes in 3 flavors)
      def alter(values: AObjs)(key: SKey): AObjs = if (inMemory) values else values.sortBy(key)._forceResult
        def _res1: AObjs = alter(res1)(z)
        def _res2: AObjs = alter(res2)(z)
        def _res3: AObjs = alter(res3)(z)

      // ===========================================================================
      input(Input).group   (p        ).by(z)                .check(_res1)
      input(Input).groupOne(_.index(0)).by(z)                .check(_res1)
      input(Input).groupOne(_.firstKey).by(z)                .check(_res1)
      input(Input).groupOne(        _.firstKey).by(_.lastKey) .check(_res1)
      input(Input).grouping(_.field(_.firstKey).by(_.lastKey)).check(_res1)

      // ---------------------------------------------------------------------------
      input(Input).groupBy (z)           .check(_res2)
      input(Input).groupBy (z).asDefault .check(_res2)
      input(Input).groupBy (z).as(_group).check(_res2)

      input(Input).grouping(_.all.by(z))           .check(_res2)
      input(Input).grouping(_.all.by(z).as(_group)).check(_res2)
      input(Input).grouping(_.all.by(z).asDefault ).check(_res2)
      input(Input).grouping(_.all.by(_.lastKey))    .check(_res2)

      input(Input).groupBy(_.lastKey)          .check(_res2)
      input(Input).groupBy(_.lastKey).asDefault.check(_res2)

      // ---------------------------------------------------------------------------
      Default61.group(g).by(f1).check(
        bobjs(
          bobj(f1 -> "foo1", g -> Seq(1, 1)),
          bobj(f1 -> "foo3", g -> Seq(3) )) )

      input(Input)
          .group(p).by(z)
          .transformAllEntities(p).using {
            _.squash(_.string(f), _.int(g)).using {
              _ .map { case (f, g) => f.size + g }
                .sum } }
        .check(_res3)

      // ===========================================================================
      input(Default52).group(g).by(f).check(
          bobjs(bobj(f -> "foo" , g -> Seq(1, 1)),
                bobj(f -> "foo2", g -> Seq(2) )) )

      // ---------------------------------------------------------------------------
      input(Default52).group(g).by(f).check(Output1)

      // ---------------------------------------------------------------------------
      input(Default52).groupBy(f)                      .check(Output2)
      input(Default52).groupBy(f).asDefault            .check(Output2)
      input(Default52).grouping(_.all.by(f)           ).check(Output2)
      input(Default52).grouping(_.all.by(f).asDefault ).check(Output2)

      // ---------------------------------------------------------------------------
      input(Default52).groupBy(f)          .as("GROUP") .check(Output3)
      input(Default52).grouping(_.all.by(f).as("GROUP")).check(Output3)

      // ===========================================================================
      input(Default52b).group("g").by("f").check(Output4)

      input(Default52c).group("g").by("f").check(alter(Output5)(f))
      input(Default52d).group("g").by("f").check(alter(Output6)(f))

      // ===========================================================================
      // renaming
      input(Default52).group(g)       .by(f).check(bobjs(bobj(f -> "foo", g -> Seq(1, 1)), bobj(f -> "foo2", g -> Seq(2) )))
      input(Default52).group(g ~> "G").by(f).check(bobjs(bobj(f -> "foo", "G" -> Seq(1, 1)), bobj(f -> "foo2", "G" -> Seq(2) )))

      // ---------------------------------------------------------------------------
      input(Default61).group(f1 ~> "F1", f2).by(g).check(bobjs(
          bobj(g -> 1, _group -> Seq(bobj("F1" -> "foo1", f2 -> "foo2"), bobj("F1" -> "foo1", f2 -> "foo2"))),
          bobj(g -> 3, _group -> Seq(bobj("F1" -> "foo3", f2 -> "foo4")))))

      input(Default61).group(f1, f2).by(g ~> "G").check(bobjs(
          bobj("G" -> 1, _group -> Seq(bobj(f1 -> "foo1", f2 -> "foo2"), bobj(f1 -> "foo1", f2 -> "foo2"))),
          bobj("G" -> 3, _group -> Seq(bobj(f1 -> "foo3", f2 -> "foo4")))))

      // ===========================================================================
      {
        val out =
          bobjs(
            bobj(f1 -> "foo1", g -> Seq(1, 1)),
            bobj(f1 -> "foo3", g -> Seq(3) ) )
        input(Default61).group(g).by(f1).check(out)
      }

      // ---------------------------------------------------------------------------
      {
        val out =
          bobjs(
              bobj(f1 -> "foo1", f2 -> "foo2", g -> Seq(1, 1)),
              bobj(f1 -> "foo3", f2 -> "foo4", g -> Seq(3) ) )

        input(Default61).group   (g       ).byTheRest.check(out)
        input(Default61).groupOne(_.lastKey).byTheRest.check(out)
      }

      // ---------------------------------------------------------------------------
      {
        val out =
          bobjs(
              bobj(g -> 1, _group -> Seq(bobj(f1 -> "foo1", f2 -> "foo2"), bobj(f1 -> "foo1", f2 -> "foo2"))),
              bobj(g -> 3, _group -> Seq(bobj(f1 -> "foo3", f2 -> "foo4"))) )

        input(Default61).group(f1, f2).by(g       )          .check(out)
        input(Default61).group(f1, f2).by(_.lastKey)          .check(out)
        input(Default61).group(f1, f2).by(_.lastKey).asDefault.check(out)

        input(Default61).group(_.initKeys     ).by(g         ).check(out)
        input(Default61).group(_.indices(0, 1)).by(g         ).check(out)
        input(Default61).group(_.initKeys     ).by(_.lastKey  ).check(out)
        input(Default61).group(_.initKeys     ).by(_.index(-1)).check(out)

        input(Default61).grouping(_.fields(_.initKeys).byTheRest).check(out)

        input(Default61).add(h -> true).group   (         _.indices(0, 1)).by(_.indices(2, 3))           .remove(h).check(out)
        input(Default61).add(h -> true).group   (         _.indices(0, 1)).by(_.indices(2, 3)).asDefault .remove(h).check(out)
        input(Default61).add(h -> true).grouping(_.fields(_.indices(0, 1)).by(_.indices(2, 3)))          .remove(h).check(out)
        input(Default61).add(h -> true).grouping(_.fields(_.indices(0, 1)).by(_.indices(2, 3)).asDefault).remove(h).check(out) } } }

// ===========================================================================