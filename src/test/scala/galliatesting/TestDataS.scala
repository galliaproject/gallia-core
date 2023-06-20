package galliatesting

import gallia._

// ===========================================================================
object TestDataS {
  import TestDataO._

  // ---------------------------------------------------------------------------
  val Default51 = bobjs(Default01, Default01b)
  val Default52 = bobjs(Default01, Default01, Default01b)

  val Default52b = bobjs(Default02, Default02z, Default02)

  val Default52c = aobjs(Default13p, Default13m, Default13p2, Default13p)
  val Default52d = aobjs(Default14p, Default14m, Default14p2, Default14p)

  val Default52ef = bobjs(Default01e, Default01f, Default01e, Default01g)

  val Default56  = bobjs(Default06a, Default06b, Default06a)
  val Default57  = aobjs(Default13p, Default13m, Default13p2, Default13p) // f.string_
  lazy val Default57b = Default57.transformString('f).using(_.size)
  
  val Default59  = aobjs(Default14p, Default14m, Default14p2, Default14p) // f.strings_

  val Default60  = bobjs(Default02, Default02z)

  val Default61  = bobjs(Default06a, Default06b, Default06a)
//private val yyy2 = bobjs(bobj('f1 -> "foo1", 'g -> 2), bobj('f1 -> "foo3", 'g -> 3 ) )

  // ===========================================================================
  // for filterby and grouping
  val res0 = bobjs(bobj('z -> true, 'p     ->     Default01)            , bobj('z -> false, 'p ->       Default01b))
  val res1 = bobjs(bobj('z -> true, 'p     -> Seq(Default01, Default01)), bobj('z -> false, 'p -> Seq  (Default01b)))
  val res2 = bobjs(bobj('z -> true, _group -> Seq(bobj('p -> Default01 ), bobj('p -> Default01))), bobj('z -> false, _group -> Seq(bobj('p -> Default01b))))
  val res3 =
    bobjs(
        bobj('z -> true , 'p -> 8),
        bobj('z -> false, 'p -> 6))

}

// ===========================================================================
