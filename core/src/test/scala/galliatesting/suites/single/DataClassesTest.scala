package galliatesting
package suites
package single

// ===========================================================================
object DataClassesTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._
  import TestMeta._
  import gallia._
  import aptus._ // for in.noneIf

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test("basic transformViaDataClass") {
      test { Default03 .transformViaDataClass[           _Default01 ] (p).using(            _.op)                     .check(bobj(p ->  4.4, z -> true)) }
      test { Default03p.transformViaDataClass[Option[    _Default01 ]](p).using(      _.map(_.op)     .getOrElse(1.1)).check(bobj(p ->  4.4, z -> true)) }
      test { Default03m.transformViaDataClass[Option[    _Default01 ]](p).using(      _.map(_.op)     .getOrElse(1.1)).check(bobj(p ->  1.1, z -> true)) }
      test { Default04 .transformViaDataClass[       Seq[_Default01 ]](p).using(      _.map(_.op).sum)                .check(bobj(p -> 11.0, z -> true)) } // 4.4+6.6=11.0
      test { Default04p.transformViaDataClass[Option[Seq[_Default01]]](p).using(_.map(_.map(_.op).sum).getOrElse(1.1)).check(bobj(p -> 11.0, z -> true)) } // 4.4+6.6=11.0
      test { Default04m.transformViaDataClass[Option[Seq[_Default01]]](p).using(_.map(_.map(_.op).sum).getOrElse(1.1)).check(bobj(p ->  1.1, z -> true)) } }

    // ---------------------------------------------------------------------------
    test("more complex transformViaDataClass") {
      val expected = bobj(p -> bobj(/*g -> 1, */f -> 3, h -> false), z -> true)

      // ---------------------------------------------------------------------------
      test { Default03.transformViaDataClass[_Default01](p).using(x => f_String(x.f)).check(bobj(p -> bobj(f -> foo), z -> true)) }

      // ---------------------------------------------------------------------------
      test { Default04.transformViaDataClass[Seq[_Default01]](p)
            .using { dc =>
              f_Int$h_Boolean(dc.head.f.size, (dc.head.f.size % 2 ) == 0) }
          .check(expected) }

        // ---------------------------------------------------------------------------
        // convoluted way
        test { Default04.cotransformViaDataClass[__Default01]
          .using { _.p.head.pipe { head =>
            $f_Int$h_Boolean(f_Int$h_Boolean(head.f.size, (head.f.size % 2 ) == 0)) } }
          .check(expected.reverseKeyOrder._forceResult) } }

    // ===========================================================================
    test("basic cotransformViaDataClass") {
      test { Default01 .cotransformViaDataClass[f_String]  .using { dc => f2_String  (dc.f      .toUpperCase)  }.check(bobj(f ->     foo,         g -> 1, f2 ->     "FOO") ) }
      test { Default02 .cotransformViaDataClass[f_Strings] .using { dc => f2_Strings (dc.f.map(_.toUpperCase)) }.check(bobj(f -> Seq(foo1, foo2), g -> 1, f2 -> Seq("FOO1", "FOO2")) ) }

      test { Default13p.cotransformViaDataClass[f_String_] .using { dc => f2_String_ (dc.f.map(_.toUpperCase)) }.check(aobj(cls(f.string_, g.int, f2.string_))(obj(f -> foo, g -> 1, f2 -> "FOO"))) }
      test { Default13m.cotransformViaDataClass[f_String_] .using { dc => f2_String_ (dc.f.map(_.toUpperCase)) }.check(aobj(cls(f.string_, g.int, f2.string_))(obj(          g -> 1))) }

      test { Default14p.cotransformViaDataClass[f_Strings_].using { dc => f2_Strings_(dc.f.map(_.map(_.toUpperCase))) }.check(aobj(cls(f.strings_, g.int, f2.strings_))(obj(f -> Seq(foo1, foo2), g -> 1, f2 -> Seq("FOO1", "FOO2")))) }
      test { Default14m.cotransformViaDataClass[f_Strings_].using { dc => f2_Strings_(dc.f.map(_.map(_.toUpperCase))) }.check(aobj(cls(f.strings_, g.int, f2.strings_))(obj(                      g -> 1))) } }

    // ===========================================================================
    test { Default01 .cotransformViaDataClass[f_String]  .as(f2).using(_.f.size).check(bobj(f ->     foo,         g -> 1, f2 -> 3) ) }
    test { Default02 .cotransformViaDataClass[f_Strings] .as(f2).using(_.f.size).check(bobj(f -> Seq(foo1, foo2), g -> 1, f2 -> 2) ) }

    test { Default13p.cotransformViaDataClass[f_String_] .as(f2).using(_.f.map(_.size).getOrElse(0)).check(aobj(cls(f.string_, g.int, f2.int ))(obj(f -> foo, g -> 1, f2 -> 3))) }
    test { Default13p.cotransformViaDataClass[f_String_] .as(f2).using(_.f.map(_.size)             ).check(aobj(cls(f.string_, g.int, f2.int_))(obj(f -> foo, g -> 1, f2 -> 3))) }
    test { Default13m.cotransformViaDataClass[f_String_] .as(f2).using(_.f.map(_.size).getOrElse(0)).check(aobj(cls(f.string_, g.int, f2.int ))(obj(          g -> 1, f2 -> 0))) }
    test { Default13m.cotransformViaDataClass[f_String_] .as(f2).using(_.f.map(_.size)             ).check(aobj(cls(f.string_, g.int, f2.int_))(obj(          g -> 1))) }

    test { Default14p.cotransformViaDataClass[f_Strings_].as(f2).using(_.f.map(_.size).getOrElse(0)).check(aobj(cls(f.strings_, g.int, f2.int ))(obj(f -> Seq(foo1, foo2), g -> 1, f2 -> 2))) }
    test { Default14p.cotransformViaDataClass[f_Strings_].as(f2).using(_.f.map(_.size)             ).check(aobj(cls(f.strings_, g.int, f2.int_))(obj(f -> Seq(foo1, foo2), g -> 1, f2 -> 2))) }
    test { Default14m.cotransformViaDataClass[f_Strings_].as(f2).using(_.f.map(_.size).getOrElse(0)).check(aobj(cls(f.strings_, g.int, f2.int ))(obj(                      g -> 1, f2 -> 0))) }
    test { Default14m.cotransformViaDataClass[f_Strings_].as(f2).using(_.f.map(_.size)             ).check(aobj(cls(f.strings_, g.int, f2.int_))(obj(                      g -> 1))) }

    // ---------------------------------------------------------------------------
    // with removal
    test { Default01.cotransformViaDataClass[f_String] .as(f2).usingWithErasing(_.f.size).check(bobj(              g -> 1, f2 -> 3) ) }

    // ===========================================================================
    test { Default01
      .cotransformViaDataClass[f_String]
        .usingWithErasing { dc => f2_Int$h_Boolean(dc.f.size, (dc.f.size % 2 ) == 0) }
          .check(bobj(g -> 1, f2 -> 3, h -> false) ) }

    // ---------------------------------------------------------------------------
    if (false) /*test */{
      TestDataO.Default03 //val Default03  = bobj(p -> Default01 , z -> true)
        .transformViaDataClass[f_String](p)
          .using { dc => Quux2c(dc.f.size, (dc.f.size % 2 ) == 0) }
            .check(bobj(p -> bobj(/*g -> 1, */f -> 3, h -> false), z -> true) ) } // --> differs from v1... (g field)
  } }

// ===========================================================================
