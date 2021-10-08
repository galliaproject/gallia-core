package gallia.atoms

import aptus.Anything_

import gallia._
import gallia.domain._
import gallia.FunctionWrappers._

// ===========================================================================
object AtomsUUTransforms {
  case class _TransformVV(pair: PathPair, f: _ff11)                     extends AtomUU { def naive(o: Obj): Obj = o.transformPathPair        (pair, f) }  
  case class _TransformWW(pair: PathPair, f: _ff11, checkType: Boolean) extends AtomUU { def naive(o: Obj): Obj = o.transformWhateverPathPair(pair, f, checkType) }

  // ---------------------------------------------------------------------------
  case class _Transform1to1 (ori: PathPair1, des: KPaths1,  f: _ff11) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f).pipe { v1  => o.put(des, v1) } }

  case class _Transform1to2 (ori: PathPair1, des: KPaths2,  f: _ff12) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f).pipe { case (v1, v2)                                  => o.put(des.entries(v1, v2) ) } }
  case class _Transform1to3 (ori: PathPair1, des: KPaths3,  f: _ff13) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f).pipe { case (v1, v2, v3)                              => o.put(des.entries(v1, v2, v3) ) } }
  case class _Transform1to4 (ori: PathPair1, des: KPaths4,  f: _ff14) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f).pipe { case (v1, v2, v3, v4)                          => o.put(des.entries(v1, v2, v3, v4) ) } }
  case class _Transform1to5 (ori: PathPair1, des: KPaths5,  f: _ff15) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f).pipe { case (v1, v2, v3, v4, v5)                      => o.put(des.entries(v1, v2, v3, v4, v5) ) } }
  case class _Transform1to6 (ori: PathPair1, des: KPaths6,  f: _ff16) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f).pipe { case (v1, v2, v3, v4, v5, v6)                  => o.put(des.entries(v1, v2, v3, v4, v5, v6) ) } }
  case class _Transform1to7 (ori: PathPair1, des: KPaths7,  f: _ff17) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f).pipe { case (v1, v2, v3, v4, v5, v6, v7)              => o.put(des.entries(v1, v2, v3, v4, v5, v6, v7) ) } }
  case class _Transform1to8 (ori: PathPair1, des: KPaths8,  f: _ff18) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f).pipe { case (v1, v2, v3, v4, v5, v6, v7, v8)          => o.put(des.entries(v1, v2, v3, v4, v5, v6, v7, v8) ) } }
  case class _Transform1to9 (ori: PathPair1, des: KPaths9,  f: _ff19) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f).pipe { case (v1, v2, v3, v4, v5, v6, v7, v8, v9)      => o.put(des.entries(v1, v2, v3, v4, v5, v6, v7, v8, v9) ) } }
  case class _Transform1to10(ori: PathPair1, des: KPaths10, f: _ff1A) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f).pipe { case (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) => o.put(des.entries(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ) } }  

  case class _Transform2to1 (ori: PathPair2,  des: KPaths1, f: _ff21) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { v => o.put(des, v) } }
  case class _Transform3to1 (ori: PathPair3,  des: KPaths1, f: _ff31) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { v => o.put(des, v) } }
  case class _Transform4to1 (ori: PathPair4,  des: KPaths1, f: _ff41) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { v => o.put(des, v) } }
  case class _Transform5to1 (ori: PathPair5,  des: KPaths1, f: _ff51) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { v => o.put(des, v) } }
  case class _Transform6to1 (ori: PathPair6,  des: KPaths1, f: _ff61) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { v => o.put(des, v) } }
  case class _Transform7to1 (ori: PathPair7,  des: KPaths1, f: _ff71) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { v => o.put(des, v) } }
  case class _Transform8to1 (ori: PathPair8,  des: KPaths1, f: _ff81) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { v => o.put(des, v) } }
  case class _Transform9to1 (ori: PathPair9,  des: KPaths1, f: _ff91) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { v => o.put(des, v) } }
  case class _Transform10to1(ori: PathPair10, des: KPaths1, f: _ffA1) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { v => o.put(des, v) } }

  case class _Transform2to2(ori: PathPair2, des: KPaths2, f: _ff22) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { case (v1, v2)     => o.put(des.entries(v1, v2    )) } }
  case class _Transform2to3(ori: PathPair2, des: KPaths3, f: _ff23) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { case (v1, v2, v3) => o.put(des.entries(v1, v2, v3)) } }
  case class _Transform3to2(ori: PathPair3, des: KPaths2, f: _ff32) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { case (v1, v2)     => o.put(des.entries(v1, v2)) } }
  case class _Transform3to3(ori: PathPair3, des: KPaths3, f: _ff33) extends AtomUU { def naive(o: Obj): Obj = ori.lookup(o).pipe(f.tupled).pipe { case (v1, v2, v3) => o.put(des.entries(v1, v2, v3)) } }
}

// ===========================================================================

