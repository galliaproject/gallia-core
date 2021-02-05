package gallia.domain

import gallia._

// ===========================================================================
case class PathPair(path: KPath, optional: Boolean) {
    def lookup(o: Obj ): AnyValue = if (optional) o.opt(path) else o.force(path)

    // ---------------------------------------------------------------------------
    def matching(value1: Any, value2: Any): Boolean =
      if (optional) value1 == Some(value2)
      else          value1 ==      value2
  }

  // ===========================================================================
  case class PathPair2(pair1: PathPair, pair2: PathPair                                                   ) { def lookup(o: Obj) = (pair1.lookup(o), pair2.lookup(o)                                                   ); def entries(v1: AnyValue, v2: AnyValue                                          ) = Seq(pair1.path -> v1, pair2.path -> v2) }
  case class PathPair3(pair1: PathPair, pair2: PathPair, pair3: PathPair                                  ) { def lookup(o: Obj) = (pair1.lookup(o), pair2.lookup(o), pair3.lookup(o)                                  ); def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue                            ) = Seq(pair1.path -> v1, pair2.path -> v2, pair3.path -> v3) }
  case class PathPair4(pair1: PathPair, pair2: PathPair, pair3: PathPair, pair4: PathPair                 ) { def lookup(o: Obj) = (pair1.lookup(o), pair2.lookup(o), pair3.lookup(o), pair4.lookup(o)                 ); def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue, v4: AnyValue              ) = Seq(pair1.path -> v1, pair2.path -> v2, pair3.path -> v3, pair4.path -> v4) }
  case class PathPair5(pair1: PathPair, pair2: PathPair, pair3: PathPair, pair4: PathPair, pair5: PathPair) { def lookup(o: Obj) = (pair1.lookup(o), pair2.lookup(o), pair3.lookup(o), pair4.lookup(o), pair5.lookup(o)); def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue, v4: AnyValue, v5: AnyValue) = Seq(pair1.path -> v1, pair2.path -> v2, pair3.path -> v3, pair4.path -> v4, pair5.path -> v5) }

// ===========================================================================