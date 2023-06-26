package galliatesting
package suites
package single

// ===========================================================================
object TranslateTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {

    test { val in: BObj = Default01
      test(in.translate(_.firstKey)   .using(foo -> "oof") check Default01c)
      test(in.translate(_.allBut(1))  .using(foo -> "oof") check Default01c)
      test(in.translate(_.allButLast) .using(foo -> "oof") check Default01c)
      test(in.translate(_.allButFirst).using(1   -> -1)    check bobj(f -> foo, g -> -1))

      test(in.translate(_.allBut(g) ).using(foo -> "oof") check Default01c)

      test(in.translate(           f ~> F ) .using(foo -> "oof") check bobj(F -> "oof", g -> 1))
      test(in.translate(_.explicit(f      )).using(foo -> "oof") check bobj(f -> "oof", g -> 1))
      //   in.translate(_.explicit(f ~> F)) .using(foo -> "oof") check bobj(F -> "oof", g -> 1) // TODO: re-allow?

      test(in.translate(f).usingLenient(foo -> "oof") check Default01c)
      test(in.translate(f).usingStrict (foo -> 3    ) check bobj(f -> 3, g -> 1))

      test(in.translate(f).using(foo -> "oof") check bobj(f -> "oof", g -> 1)) }

    // ---------------------------------------------------------------------------
    test { val in: BObj = Default10
      test(in.translate(_.ifString           ).usingLenient("" -> ".", "@@@" -> "@") check bobj(f -> ".", g -> 1, h -> ".", p -> bobj("pf" -> "" , "pg" -> 2)))
      test(in.translate(_.ifStringRecursively).usingLenient("" -> ".", "@@@" -> "@") check bobj(f -> ".", g -> 1, h -> ".", p -> bobj("pf" -> ".", "pg" -> 2)))

      test(in.translate(_.soleKey).usingLenient("" -> 0) metaError ErrorId.MoreThanOneKey)
      //bobj(f -> "", g -> 1, h -> "", p -> bobj(pf -> "", pg -> 2))
      test(in.translate(_.ifString           ).usingLenient("" -> 0) metaError ("MustBeSameType", "-"))
      test(in.translate(_.ifString           ).usingStrict ("" -> 0) check bobj(f -> 0, g -> 1, h -> 0, p -> bobj("pf" -> "", "pg" -> 2)))
      test(in.translate(_.ifStringRecursively).usingStrict ("" -> 0) check bobj(f -> 0, g -> 1, h -> 0, p -> bobj("pf" ->  0, "pg" -> 2)))
      /*in.applyRecursivelyIfValue[String](_.startsWith("f")).using(_.toUpperCase) */ }

    // ---------------------------------------------------------------------------
    test {
      test(Default10.translate(_.soleKey).usingLenient(foo -> "oof") metaError ErrorId.MoreThanOneKey)
      test(Default11.translate(_.soleKey).usingLenient(foo -> "oof") check bobj(f -> "oof"))
      test(Default11.translate(_.soleKey).usingStrict (foo -> 3    ) check bobj(f -> 3))

      test(Default06.translate(_.indices(0,  1)).using(foo -> "oof") check bobj(f1 -> "oof", f2 -> "oof", g -> 1))
      test(Default06.translate(_.indices(0, -2)).using(foo -> "oof") check bobj(f1 -> "oof", f2 -> "oof", g -> 1))
      test(Default06.translate(_.indices(0,  5)).using(foo -> "oof") metaError ErrorId.OutOfBoundKey)
      test(Default06.translate(_.indices(0, -5)).using(foo -> "oof") metaError ErrorId.OutOfBoundKey)
    //Default06.translate(_.indices(0,  5)).using(foo -> "oof") fail ErrorId.OutOfBound
    //Default06.translate(_.indices(0, -5)).using(foo -> "oof") fail ErrorId.OutOfBound

      test(Default03.translate(p |> f).using(foo -> "oof") check bobj(p ->     Default01c,              z -> true))
      test(Default04.translate(p |> f).using(foo -> "oof") check bobj(p -> Seq(Default01c, Default01b), z -> true))

      test(Default03.translate(p |> f ~> F).using(foo -> "oof") check bobj(p -> bobj(F -> "oof", g -> 1), z -> true)) }

    // ---------------------------------------------------------------------------
    test("rename dynamically") {
      test(Default01.rename { skey => if (skey == "g") "f" else skey } metaError ErrorId.FieldAlreadyExists)

      test(Default01.rename           (_.toUpperCase) check             bobj(F -> foo, "G" -> 1))
      test(Default03.renameRecursively(_.toUpperCase) check bobj("P" -> bobj(F -> foo, "G" -> 1), "Z" -> true))

      // ---------------------------------------------------------------------------
      if (false) // TODO: catch error properly, right now it selected Seq(Symbol(""), Symbol(""))
        bobj(f -> foo, "gg" -> 1).rename(_.tail.tail) metaError gallia.vldt.ErrorId.CouldNotRenameDynamically // "f - empty.tail"

      // ---------------------------------------------------------------------------
      val in = bobj(f -> foo, "gg" -> 1)
        test(in.noop(_.forEachKey(_.customKeys(_.tail.tail)).thn((u, k) => u.rename(k ~> k.name.toUpperCase))))
        test(in       .forEachKey(_.customKeys(_.tail     )).thn((u, k) => u.rename(k ~> k.name.toUpperCase)) check bobj(f -> foo, "GG" -> 1)) } }
}

// ===========================================================================