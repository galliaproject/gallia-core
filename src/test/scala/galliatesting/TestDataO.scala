package galliatesting

import aptus._
import gallia._

// ===========================================================================
object TestDataO extends TestDataO

// ---------------------------------------------------------------------------
trait TestDataO {
  val Default00  = bobj(g -> 1)
  val Default00a = bobj(g -> 1)
  val Default00b = bobj(g -> 2)
  
  val Default00f  = bobj(f -> foo)

  // ---------------------------------------------------------------------------
  val Default01    = bobj(f ->  foo , g -> 1)
    val Default01b = bobj(f ->  foo2, g -> 2)
    val Default01c = bobj(f -> "oof", g -> 1)

    val Default01e = bobj(f ->  foo , g -> 1.1)
    val Default01f = bobj(f ->  foo2, g -> 2.2)
    val Default01g = bobj(f ->  foo , g -> 2.2)

  // ---------------------------------------------------------------------------
  val Default02    = bobj(f -> Seq(foo1, foo2), g -> 1)

    val Default02z = bobj(f -> Seq(foo3, foo4), g -> 3)

    val Default02y = bobj(f -> Seq("1oof", "2oof"), g -> 1)
    val Default02x = bobj(f -> Seq("2oof", "1oof"), g -> 1)

    val Default02b = bobj(f -> Seq(1  , 2  , 3  ), g -> 1)
    val Default02c = bobj(f -> Seq(1.1, 2.2, 3.3), g -> 1)

  // ---------------------------------------------------------------------------
  val Default03  = bobj(p -> Default01 , z -> true)

    val Default03b = bobj(p -> bobj(f1 -> 1, f2 -> 2), "f3" -> 3, z -> true)

    // ---------------------------------------------------------------------------
    val Default03p = aobj(
      cls(p   .cls_(f.string, g.int ), z.boolean))(
      obj(p -> obj (f -> foo, g -> 1), z -> true) )

    val Default03m = aobj(
      cls(p   .cls_(f.string, g.int   ), z.boolean))(
      obj(                               z -> true) )

  val Default04 = bobj(p -> Seq(Default01, Default01b), z -> true)

    // ---------------------------------------------------------------------------
    val Default04p = aobj(
      cls(p   .clss_  (f.string, g.int),                            z.boolean))(
      obj(p -> Seq(obj(f -> foo, g -> 1), obj(f -> foo2, g -> 2)) , z -> true) )

    val Default04m = aobj(
      cls(p   .clss_(f.string, g.int), z.boolean))(
      obj(                             z -> true) )

  // ---------------------------------------------------------------------------
  val Default06    = bobj(f1 -> foo , f2 -> foo , g -> 1)
    val Default06a = bobj(f1 -> foo1, f2 -> foo2, g -> 1)
    val Default06b = bobj(f1 -> foo3, f2 -> foo4, g -> 3)

  // ---------------------------------------------------------------------------
  val Default09 = bobj(p -> Default01, z -> true, f -> foo)

  val Default10 = bobj(f -> "", g -> 1, h -> "", p -> bobj("pf" -> "", "pg" -> 2))

  val Default11 = bobj(f -> foo)

  // ===========================================================================
  val Default13p  = aobj(cls(f.string_, g.int))(obj(f -> foo, g -> 1))
  val Default13m  = aobj(cls(f.string_, g.int))(obj(          g -> 1))

  val Default14p  = aobj(cls(f.strings_, g.int))(obj(f -> Seq(foo1, foo2), g -> 1))
  val Default14m  = aobj(cls(f.strings_, g.int))(obj(                      g -> 1))

val Default13p2 = aobj(cls(f.string_ , g.int))(obj(f ->      bar           , g -> 2))
val Default14p2 = aobj(cls(f.strings_, g.int))(obj(f -> Seq("bar1", "bar2"), g -> 2))

  // ===========================================================================
  val Default15p = aobj(
      cls(f.string_, g.int , h.boolean))(
      obj(f -> foo , g -> 1, h -> true))

  val Default15m = aobj(
      cls(f.string_, g.int , h.boolean))(
      obj(           g -> 1, h -> true))

  // ---------------------------------------------------------------------------
  val Default16p = aobj(
      cls(f.strings_          , g.int , h.boolean))(
      obj(f -> Seq(foo1, foo2), g -> 1, h -> true))

  val Default16m = aobj(
      cls(f.strings_, g.int , h.boolean))(
      obj(            g -> 1, h -> true))

  // ===========================================================================
  val Default17 = bobj("value" -> "2022-03-17".parseLocalDate)
}

// ===========================================================================
