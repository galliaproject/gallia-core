package gallia.atoms

import aptus.Anything_

import gallia._
import gallia.domain._
import gallia.FunctionWrappers._

// ===========================================================================
object AtomsUUTransforms {
  case class _TransformVV(pair: PathPair, f: _ff11) extends AtomUU { def naive(o: Obj): Obj = o.transformPathPair(pair, f) }

  // ---------------------------------------------------------------------------
  case class _Transform1to1(ori: PathPair1, des: KPaths1, f: _ff11) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f).thn { v1  => o.put(des, v1) } }

  case class _Transform1to2(ori: PathPair1, des: KPaths2, f: _ff12) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f).thn { case (v1, v2)             => o.put(des.entries(v1, v2) ) } }
  case class _Transform1to3(ori: PathPair1, des: KPaths3, f: _ff13) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f).thn { case (v1, v2, v3)         => o.put(des.entries(v1, v2, v3) ) } }
  case class _Transform1to4(ori: PathPair1, des: KPaths4, f: _ff14) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f).thn { case (v1, v2, v3, v4)     => o.put(des.entries(v1, v2, v3, v4) ) } }
  case class _Transform1to5(ori: PathPair1, des: KPaths5, f: _ff15) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f).thn { case (v1, v2, v3, v4, v5) => o.put(des.entries(v1, v2, v3, v4, v5) ) } }

  case class _Transform2to1(ori: PathPair2, des: KPaths1, f: _ff21) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f.tupled).thn { v1 => o.put(des, v1) } }
  case class _Transform3to1(ori: PathPair3, des: KPaths1, f: _ff31) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f.tupled).thn { v1 => o.put(des, v1) } }
  case class _Transform4to1(ori: PathPair4, des: KPaths1, f: _ff41) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f.tupled).thn { v1 => o.put(des, v1) } }
  case class _Transform5to1(ori: PathPair5, des: KPaths1, f: _ff51) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f.tupled).thn { v1 => o.put(des, v1) } }

  case class _Transform2to2(ori: PathPair2, des: KPaths2, f: _ff22) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f.tupled).thn { case (v1, v2)     => o.put(des.entries(v1, v2    )) } }
  case class _Transform2to3(ori: PathPair2, des: KPaths3, f: _ff23) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f.tupled).thn { case (v1, v2, v3) => o.put(des.entries(v1, v2, v3)) } }
  case class _Transform3to2(ori: PathPair3, des: KPaths2, f: _ff32) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f.tupled).thn { case (v1, v2)     => o.put(des.entries(v1, v2)) } }
  case class _Transform3to3(ori: PathPair3, des: KPaths3, f: _ff33) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).thn(f.tupled).thn { case (v1, v2, v3) => o.put(des.entries(v1, v2, v3)) } }
}

// ===========================================================================

