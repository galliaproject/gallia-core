package galliatesting
package suites
package single

import gallia.meta.SubInfo._

// ===========================================================================
object UnionTypeTest extends utest.TestSuite with GalliaTestSuite with utils.FormerSandboxImplicits with TestDataO { import utest._
  import gallia._

	// ---------------------------------------------------------------------------
	implicit class Float_(u: Float) { def incrementFloat: Float = u + 1 }

  // ---------------------------------------------------------------------------
  private val FOO1 = "FOO1"
  private val FOO2 = "FOO2"

  // ===========================================================================
  val tests = Tests {

    test {
      val c = cls(f.requiredUnion(string, int))

      test(       """{"f":  3}""".read(_.schema(c))._forceResult.forceInt(f).ensuring(_ == 3))               // test gson -> gallia only
      test(aobj(c)(obj(f -> 3)).formatCompactJson                           .ensuring(_ == """{"f":3}""")) } // test gallia -> gson only

    // ---------------------------------------------------------------------------
    test("""{"f": "foo", "g": 1.1}""".read()._assert2(bobj(f -> foo, g -> 1.1)))
    test("""{"f": "foo", "g": 1  }""".read()._assert2(bobj(f -> foo, g -> 1)))

    // ---------------------------------------------------------------------------
    test(          bobj(f -> foo, g -> 1) .increment(g)     ._assert2(          bobj(f -> foo, g -> 2)))
    test(bobj(p -> bobj(f -> foo, g -> 1)).increment(p |> g)._assert2(bobj(p -> bobj(f -> foo, g -> 2))))

    // ---------------------------------------------------------------------------
    test(aobj(      cls(f.string, g.int)) (         obj(f -> foo, g -> 1)) .increment(g)     ._assert2(aobj(      cls(f.string, g.int)) (         obj(f -> foo, g -> 2))))
    test(aobj(cls(p.cls(f.string, g.int)))(obj(p -> obj(f -> foo, g -> 1))).increment(p |> g)._assert2(aobj(cls(p.cls(f.string, g.int)))(obj(p -> obj(f -> foo, g -> 2)))))

    // ===========================================================================
    test {
      val ca = cls(f.int)
      val cb = cls(f.string)

      test(unionTest2(ca)("""{"f":  3   }""", 4  ) { _.increment(f) })
      test(unionTest2(ca)("""{"f":  3   }""", 4  ) { _.transform(_.int   (f)).using(_ + 1) })
      test(unionTest2(cb)("""{"f": "foo"}""", FOO) { _.transform(_.string(f)).using(_.toUpperCase) }) }

    // ---------------------------------------------------------------------------
val c1 = cls(f.requiredUnion(string, int))
    test {
        test(unionTest1(c1)("""{"f": "foo"}""", FOO))
        test(unionTest1(c1)("""{"f":    3 }""", 3)) }

    // ---------------------------------------------------------------------------
    test {
      val c = cls(f.requiredUnion(string, single(_._Float)))

      test(unionTest1(c)("""{"f": "foo"}""", FOO))
      test(unionTest1(c)("""{"f":  3.3 }""", 3.3.toFloat))
      test(unionTest2(c)("""{"f": "foo"}""", foo)                  { _.transform(_.float(f)).using(_.incrementFloat) })
      test(unionTest3(aobj(c)(obj(f -> 3.3.toFloat)))(4.3.toFloat) { _.transform(_.float(f)).using(_.incrementFloat) }) }

    // ---------------------------------------------------------------------------
    test("transformUU") {
      val c =
          cls(f.requiredUnion(
            string,
            single(cls(f.string, g.int))))
        test(unionTest1(c)("""{"f":       "foo"}"""         ,          FOO))
        test(unionTest2(c)("""{"f": {"f": "foo", "g": 1}}""", obj(f -> FOO, g -> 1)) { _.transformEntity(f).using(_.toUpperCase(f)) })
        test(unionTest2(c)("""{"f": {"f": "foo", "g": 1}}""", obj(f -> foo, g -> 2)) { _.transformEntity(f).using(_.increment  (g)) }) }

      // ---------------------------------------------------------------------------
      test("transformUZ") {
        val c =
            cls(f.requiredUnion(
              string,
              multiple(cls(f.string, g.int))))

          test(unionTest1(c)("""{"f":        "foo"}""",                                                FOO))
          test(unionTest2(c)("""{"f": [{"f": "foo1", "g": 1}, {"f": "foo2", "g": 2}]}""", Seq(obj(f -> FOO1, g -> 1), obj(f -> FOO2, g -> 2))) { _.transformAllEntities(f).using(_.toUpperCase(f)) })
          test(unionTest2(c)("""{"f": [{"f": "foo1", "g": 1}, {"f": "foo2", "g": 2}]}""", Seq(obj(f -> foo1, g -> 2), obj(f -> foo2, g -> 3))) { _.transformAllEntities(f).using(_.increment  (g)) }) }

    // ---------------------------------------------------------------------------
    test {
      val c =
            cls(f.requiredUnion(
              int,
              single(cls(name = "type1", Seq("_type".string, f1.string, g.int))),
              single(cls(name = "type2", Seq("_type".string, f2.string, h.int)))))

        test {
          test(unionTest2(c)("""{"f":                                       3 }""",                                          3 ) { identity })
          test(unionTest2(c)("""{"f": {"_type": "type1", "f1": "foo1", "g": 1}}""", obj("_type" -> "type1", f1 -> foo1, g -> 1)) { identity })
          test(unionTest2(c)("""{"f": {"_type": "type2", "f2": "foo2", "h": 3}}""", obj("_type" -> "type2", f2 -> foo2, h -> 3)) { identity }) }

        test {
          test(unionTest2(c)("""{"f":                                       3 }""",                                          4 ) { _.transform(_.int(f)).using(_ + 1) })
          test(unionTest2(c)("""{"f": {"_type": "type1", "f1": "foo1", "g": 1}}""", obj("_type" -> "type1", f1 -> FOO1, g -> 1)) { _.transformEntity(f).withTypeName("type1").using(_.toUpperCase(f1)) })
          test(unionTest2(c)("""{"f": {"_type": "type1", "f1": "foo1", "g": 1}}""", obj("_type" -> "type1", f1 -> foo1, g -> 2)) { _.transformEntity(f).withTypeName("type1").using(_.increment  (g))  })
          test(unionTest2(c)("""{"f": {"_type": "type2", "f2": "foo2", "h": 3}}""", obj("_type" -> "type2", f2 -> FOO2, h -> 3)) { _.transformEntity(f).withTypeName("type2").using(_.toUpperCase(f2)) })
          test(unionTest2(c)("""{"f": {"_type": "type2", "f2": "foo2", "h": 3}}""", obj("_type" -> "type2", f2 -> foo2, h -> 4)) { _.transformEntity(f).withTypeName("type2").using(_.increment  (h))  }) } }

    // ---------------------------------------------------------------------------
    test {
      val c =
        cls(f.requiredUnion(
          string, // only difference
          single(cls(name = "type1", Seq("_type".string, f1.string, g.int))),
          single(cls(name = "type2", Seq("_type".string, f2.string, h.int)))))

      test(unionTest2(c)("""{"f":                          "foo"          }""",                               FOO          ) { _.transform(_.string(f)).using(_.toUpperCase) })
      test(unionTest2(c)("""{"f": {"_type": "type1", "f1": "foo1", "g": 1}}""", obj("_type" -> "type1", f1 -> FOO1, g -> 1)) { _.transformEntity(f).withTypeName("type1").using(_.toUpperCase(f1)) })
      test(unionTest2(c)("""{"f": {"_type": "type1", "f1": "foo1", "g": 1}}""", obj("_type" -> "type1", f1 -> foo1, g -> 2)) { _.transformEntity(f).withTypeName("type1").using(_.increment  (g))  })
      test(unionTest2(c)("""{"f": {"_type": "type2", "f2": "foo2", "h": 3}}""", obj("_type" -> "type2", f2 -> FOO2, h -> 3)) { _.transformEntity(f).withTypeName("type2").using(_.toUpperCase(f2)) })
      test(unionTest2(c)("""{"f": {"_type": "type2", "f2": "foo2", "h": 3}}""", obj("_type" -> "type2", f2 -> foo2, h -> 4)) { _.transformEntity(f).withTypeName("type2").using(_.increment  (h))  }) }

    // ---------------------------------------------------------------------------
    test("using index/hint") {
      val c =
        cls(f.requiredUnion(
          int,
          single(cls(f1.string, g.int)),
          single(cls(f2.string, h.int))))

        test(unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj(f1 -> foo1, g -> 1)) { identity })

        // ---------------------------------------------------------------------------
        test(unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj(f1 -> FOO1, g -> 1)) { _.transformEntity(f).withIndex(0).using(_.toUpperCase(f1)) })

          test(unionTest2(c)("""{"f": 3                     }""", 3                      ) { _.transformEntity(f).withIndex(1).using(_.toUpperCase(f2)) })
          test(unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj(f1 -> foo1, g -> 1)) { _.transformEntity(f).withIndex(1).using(_.toUpperCase(f2)) })
          test(unionTest2(c)("""{"f": {"f2": "foo2", "h": 2}}""", obj(f2 -> FOO2, h -> 2)) { _.transformEntity(f).withIndex(1).using(_.toUpperCase(f2)) })

          // ---------------------------------------------------------------------------
          test(unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj(f1 -> FOO1, g -> 1)) { _.transformEntity(f).withFieldHint(f1).using(_.toUpperCase(f1)) })

          test(unionTest2(c)("""{"f": 3                     }""", 3                      ) { _.transformEntity(f).withFieldHint(f2).using(_.toUpperCase(f2)) })
          test(unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj(f1 -> foo1, g -> 1)) { _.transformEntity(f).withFieldHint(f2).using(_.toUpperCase(f2)) })
          test(unionTest2(c)("""{"f": {"f2": "foo2", "h": 2}}""", obj(f2 -> FOO2, h -> 2)) { _.transformEntity(f).withFieldHint(f2).using(_.toUpperCase(f2)) })

        // ---------------------------------------------------------------------------
        test(unionTest2(c)("""{"f": {"f2": "foo2", "h": 2}}""", obj(f2 -> FOO2, h -> 2)) { _.transformEntity(f).withFieldHint(f2).using(_.toUpperCase(f2)) })

        test(unionTest2(c)("""{"f": {"f2": "foo2", "h": 2}}""", obj(f2 -> FOO2, h -> 2)) { _.transformEntity(f).withPredicate(c => c.contains(f2) && c.contains(h), _.string_(f2).exists(_ == foo2)).using(_.toUpperCase(f2)) })

  //    test(unionTest2(c)("""{"f": {"f1": "foo1", "g": 1}}""", obj(f1 -> FOO1, g -> 1)) { _.transformEntity(f).using(_.toUpperCase(f1)) })
    }

    // ===========================================================================
    test ("using transformx") {
      test(unionTest2(c1)("""{"f": "foo"}""", foo) { _.increment(f) })
      test(unionTest2(c1)("""{"f":  3   }""",  4 ) { _.increment(f) }) }

    // ===========================================================================
    test("to non-union") {
      test("""{"f":  3   }""".read(_.schema(c1)).transform(_.int   (f)).using(_.toString)                      ._assert(bobj(f -> "3")))
      test("""{"f":  3   }""".read(_.schema(c1)).transform(_.int   (f)).using(_.toString).assertNotUnionType(f)._assert(bobj(f -> "3"))) // recommended
      test("""{"f":  3   }""".read(_.schema(c1))                                         .assertIsUnionType (f)._silentRun())

      test("""{"f": "foo"}""".read(_.schema(c1)).transform(_.string(f)).using(_.length)                        ._assert(bobj(f ->  3 )))
      test("""{"f": "foo"}""".read(_.schema(c1)).transform(_.string(f)).using(_.length)  .assertNotUnionType(f)._assert(bobj(f ->  3 ))) // recommended
      test("""{"f": "foo"}""".read(_.schema(c1))                                         .assertIsUnionType (f)._silentRun()) }
//todo ; error out

    // ===========================================================================
    val bb  = aobj(cls(f1.string_ , f2.int_, h.boolean))(obj(f1 -> foo,                      h -> _t))
    val bb2 = aobj(cls(f1.strings_, f2.int_, h.boolean))(obj(f1 -> Seq(foo1, foo2),          h -> _t))
    val cc  = aobj(cls(f1.string_ , f2.int_, h.boolean))(obj(                       f2 -> 3, h -> _t))

    // ---------------------------------------------------------------------------
    val aa  = aobj(cls(f.requiredUnion(string,  int), h.boolean))(obj(f -> foo,             h -> _t))
    val aa2 = aobj(cls(f.requiredUnion(strings, int), h.boolean))(obj(f -> Seq(foo1, foo2), h -> _t))

    val dd  = aobj(cls(f.requiredUnion(string,  int), h.boolean))(obj(f -> 3,               h -> _t))

    // ---------------------------------------------------------------------------
    test("fuseToUnion") {
      test(bb.fuseToUnion(f1, f2).as(f)._assert(aa))
      test(cc.fuseToUnion(f1, f2).as(f)._assert(dd)) }
//    aobj(cls(f1.string_, f2.int_, h.boolean))(obj(f1 -> foo, f2 -> 3, h -> _t)).foo(f1, f2, f)._fail()
//    aobj(cls(f1.string_, f2.int_, h.boolean))(obj(                    h -> _t)).foo(f1, f2, f)._fail()

    // ---------------------------------------------------------------------------
    test("fissionFromUnion") {
      test(aa .fissionFromUnion(f).as(f1, f2)._assert(bb))
      test(dd .fissionFromUnion(f).as(f1, f2)._assert(cc))
      test(aa2.fissionFromUnion(f).as(f1, f2)._assert(bb2)) } }

  // ===========================================================================
  private def unionTest1[T: __WTT_for_testing_only](c: Cls)(s: String, t: T) = unionTest2(c)(s, t) { _.transform(_.string(f)).using(_.toUpperCase) }

    // ---------------------------------------------------------------------------
    private def unionTest2[T: __WTT_for_testing_only](c: Cls)(s: String, t: T)(f: HeadO => HeadO) = {
      s .read(_.schema(c)).pipe(f)
        ._assert2(
          origin   = s.read(_.schema(c))._forceResult,
          expected = AObj(c, obj("f" -> t))) }

    // ---------------------------------------------------------------------------
    private def unionTest3[T: __WTT_for_testing_only](input: AObj)(t: T)(f: HeadO => HeadO) = {
      input
        .identity.pipe(f)
        ._assert2(AObj(input.c, obj("f" -> t))) }
}

// ===========================================================================