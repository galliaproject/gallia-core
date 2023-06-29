package galliatesting
package suites
package single

// ===========================================================================
object SetDefaultValueForTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {

    test { val in = TestDataO.Default13m
      test(in.setDefaultFor(f)                       .asValue("default").check(bobj(f -> "default", g -> 1)))
      test(in.setDefaultFor(_.ifOptionalString)      .asValue("default").check(bobj(f -> "default", g -> 1)))
      test(in.setDefaultFor(_.ifType[Option[String]]).asValue("default").check(bobj(f -> "default", g -> 1))) /* bit awkward... */ }

    // ===========================================================================
    test {
      val inM = TestDataO.Default15m
      val inP = TestDataO.Default15p

      // ---------------------------------------------------------------------------
      test(inM.setDefaultFor(f ~> F   ).asValue("-").check(bobj(F -> "-", g -> 1, h -> _t)))
      test(inM.setDefaultFor(f ~> F, g).asValue("-").metaError(ErrorId.TypeMismatch))

      test(aobj(cls(p.cls(f.string_, g.int), z.boolean))(obj(p -> obj(g -> 1), z -> _t)).setDefaultFor(p |> f).asValue(foo).check(Default03))

      test(inM.forKey(_.firstKey).thn(_.setDefaultFor(_).asValue("-")).check(bobj(f -> "-", g -> 1, h -> _t)))

      test(inM.setDefaultFor(_.firstKey).asValue("-").check(bobj(f -> "-", g -> 1, h -> _t)))
      test(inP.setDefaultFor(f         ).asValue("-").check(bobj(f -> foo, g -> 1, h -> _t)))
      test(inP.setDefaultFor(f ~> F    ).asValue("-").check(bobj(F -> foo, g -> 1, h -> _t)))

      test(inM.setDefaultFor(_.allButLast).asValue("-").metaError(ErrorId.TypeMismatch))
      inM.setDefaultFor(
          f,
          g ~> "G",
          p |> h,
          p |> "i" ~> "I")
        .asValue("-") /* compiles */ }
 }
}

// ===========================================================================