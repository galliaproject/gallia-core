package galliatesting
package suites
package single

// ===========================================================================
object NestingRelatedTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test("nest") {
      test(Default06.nest(f1    ).under(F).check(bobj(f2 -> "foo", g -> 1, F -> bobj(f1 -> "foo"))))
      test(Default06.nest(f1, f2).under(F).check(bobj(             g -> 1, F -> bobj(f1 -> "foo", f2 -> "foo"))))

      test(Default06.nest(f1 ~> "F1", f2 ~> "F2").under(F).check(bobj(g -> 1, F -> bobj("F1" -> "foo", "F2" -> "foo"))))
      test(Default06.nest(f1 ~> "F1", f2)        .under(F).check(bobj(g -> 1, F -> bobj("F1" -> "foo", f2 -> "foo"))))

      test(Default06.nest(f1 ~> "F1", f2 ~> "F2").into(F).check(bobj(g -> 1, F -> bobj("F1" -> "foo", "F2" -> "foo"))))
      test(Default06.nest(f1 ~> "F1", f2)        .into(F).check(bobj(g -> 1, F -> bobj("F1" -> "foo", f2 -> "foo"))))

      //bobj("pp" -> Default06).nest("pp" |> f1).under(???)

      test(Default03.nest(z).into(p).check(bobj(p -> bobj(f -> "foo", g -> 1, z -> true))))

      // ---------------------------------------------------------------------------
      test(Default06.nest(f1     )        .under(F).check(bobj(f2 -> "foo", g -> 1, F -> bobj(f1 -> "foo"))))
      test(Default06.nest(_.firstKey)     .under(F).check(bobj(f2 -> "foo", g -> 1, F -> bobj(f1 -> "foo"))))
      test(Default06.nest(_.allBut(f2, g)).under(F).check(bobj(f2 -> "foo", g -> 1, F -> bobj(f1 -> "foo"))))

      test(Default06.nest(f1     )        .under(F).check(bobj(f2 -> "foo", g -> 1, F -> bobj(f1 -> "foo"))))

      test(Default03.nest(z).into(p ~> "P").check(bobj("P" -> bobj(f -> "foo", g -> 1, z -> true)))) }

    // ===========================================================================
    test("unnest") {
      test(Default03.unnestFrom(p).field(f)     .check(bobj(p -> bobj(g -> 1), z -> true, f -> "foo")))
      test(Default03.unnestFrom(p).field(f ~> F).check(bobj(p -> bobj(g -> 1), z -> true, F -> "foo")))

      test(Default03.unnestAllFrom(p).check(bobj(z -> true, f -> "foo", g -> 1)))

      test(Default01.unnestFrom(f).field(g).metaError("210109145953" -> ""))
      test(Default04.unnestFrom(p).field(g).metaError("210109145954" -> ""))

      // ---------------------------------------------------------------------------
      // regression test ciphia bug
      test(aobj(cls(p.cls(f.string_, g.int)))(obj(p -> obj(f -> "foo", g -> 1))).unnestFrom("p").field("f").display())
      test(aobj(cls(p.cls(f.string_, g.int)))(obj(p -> obj(            g -> 1))).unnestFrom("p").field("f").display())

      // ---------------------------------------------------------------------------
      val OooIn  = bobj(p -> Seq(Default00, Default00b), z -> true)
      val OooOut = bobj(z -> true, g -> Seq(1, 2))

      test(OooIn.unnestFrom(p).field (g)        .check(OooOut))
      test(OooIn.unnestFrom(p).fields(_.soleKey).check(OooOut))
      test(OooIn.unnestAllFrom(p)               .check(OooOut))

      // ---------------------------------------------------------------------------
      val UnnestIn  = bobj("a" -> Seq(bobj(h -> 1), bobj(h -> 2)))
      val UnnestOut = bobj("a" -> Seq(1,2))

      test(UnnestIn.unnestAllFrom("a").rename(h ~> "a").check(UnnestOut))
      test(UnnestIn.unnestOOO ("a")                    .check(UnnestOut)) }

    // ===========================================================================
    test("renest") {
      val in  = bobj("foo_bar" -> "x", "foo_baz" -> "y", "foo_qux" -> "z")

        test(in.renest("foo_bar", "foo_baz")            .usingDefaultSeparator.check(bobj("foo_qux" -> "z", "foo" -> bobj("bar" -> "x", "baz" -> "y"))))
        test(in.renest(_.filterKeys { key =>
          key.startsWith("f") && !key.endsWith("qux") }).usingDefaultSeparator.check(bobj("foo_qux" -> "z", "foo" -> bobj("bar" -> "x", "baz" -> "y"))))
        test(in.renest(_.allKeys         )              .usingDefaultSeparator.check(bobj(                  "foo" -> bobj("bar" -> "x", "baz" -> "y", "qux" -> "z"))))

      // ---------------------------------------------------------------------------
      test(bobj("a_b_c" -> 1)             .renestAllKeys.usingDefaultSeparator.check(bobj("a" -> bobj("b" -> bobj("c" -> 1)))))
      test(bobj("x_A" -> 1, "y_B" -> true).renestIfKeys(_.startsWith("x")).usingSeparator("_").check(bobj("y_B" -> true, "x" -> bobj("A" -> 1)))) }

    // ===========================================================================
    test("nest into + denormalization") {
      test(Default04.nest("z").into("p").check(bobj(p -> Seq(
          bobj(f -> "foo",  g -> 1, z -> true),
          bobj(f -> "foo2", g -> 2, z -> true))))) }
  }
}

// ===========================================================================