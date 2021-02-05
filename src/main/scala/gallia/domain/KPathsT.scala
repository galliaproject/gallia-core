package gallia.domain

import gallia.AnyValue
import gallia.KPath

// ===========================================================================
case class KPaths2(path1: KPath, path2: KPath)                                           { def entries(v1: AnyValue, v2: AnyValue                                          ) = Seq(path1 -> v1, path2 -> v2) }
case class KPaths3(path1: KPath, path2: KPath, path3: KPath)                             { def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue                            ) = Seq(path1 -> v1, path2 -> v2, path3 -> v3) }
case class KPaths4(path1: KPath, path2: KPath, path3: KPath, path4: KPath)               { def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue, v4: AnyValue              ) = Seq(path1 -> v1, path2 -> v2, path3 -> v3, path4 -> v4) }
case class KPaths5(path1: KPath, path2: KPath, path3: KPath, path4: KPath, path5: KPath) { def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue, v4: AnyValue, v5: AnyValue) = Seq(path1 -> v1, path2 -> v2, path3 -> v3, path4 -> v4, path5 -> v5) }

// ===========================================================================