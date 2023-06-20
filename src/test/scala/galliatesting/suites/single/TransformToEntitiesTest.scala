package galliatesting
package suites
package single

// ===========================================================================
object TransformToEntitiesTest extends utest.TestSuite with GalliaTestSuite { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test("transform to entity") {
      TestDataO.Default01.transform(_.string("f"))
        .withSchema("size".int, "letters".string)
          .using { s =>
            obj("size" -> s.size, "letters" -> s.toUpperCase.distinct.sorted) }
        .check(bobj(f -> bobj("size" -> 3, "letters" -> "FO"), g -> 1)) }

    // ---------------------------------------------------------------------------
    // TODO: t230619162239 - offer shorthand version like so (will need intermediate class):
    //    test("transform to entity 2") {
    //      TestDataO.Default01.transform(_.string("f"))
    //          .using(
    //             "size"    -> _.size,
    //             "letters" -> _.toUpperCase.distinct.sorted)
    //        .check(bobj(f -> bobj("size" -> 3, "letters" -> "FO"), g -> 1)) }

    // ---------------------------------------------------------------------------
    test("transform to entities") {
      TestDataO.Default01.transform(_.string("f"))
        .withSchema("size".int, "letters".string)
          .using { s =>
            Seq(
              obj("size" ->  s.size, "letters" -> s.toUpperCase.distinct.sorted),
              obj("size" -> -s.size, "letters" -> s.toLowerCase.distinct.sorted)) }
        .check(bobj(
          f -> Seq(
            bobj("size" ->  3, "letters" -> "FO"),
            bobj("size" -> -3, "letters" -> "fo")),
          g -> 1)) }

  } }

// ===========================================================================