package gallia
package domain

import vldt.MetaValidation

// ===========================================================================
trait KPathSeq {
    def paths: Seq[KPath]
    
    def vldtAsNewDestination(c: Cls): Errs =
      paths.flatMap(MetaValidation.fieldAbsence(c, _)) ++ 
      MetaValidation.distinctKPaths(paths)
  }

  // ---------------------------------------------------------------------------
  case class KPaths2(path1: KPath, path2: KPath) extends KPathSeq {
	  override val paths: Seq[KPath] = Seq(path1, path2)
    def entries(v1: AnyValue, v2: AnyValue) = Seq(path1 -> v1, path2 -> v2)
  }

  // ---------------------------------------------------------------------------
  case class KPaths3 (path1: KPath, path2: KPath, path3: KPath)                                                                                                    extends KPathSeq { def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue)                                                                                                    = Seq(path1 -> v1, path2 -> v2, path3 -> v3);                                                                                              override val paths: Seq[KPath] = Seq(path1, path2, path3) }
  case class KPaths4 (path1: KPath, path2: KPath, path3: KPath, path4: KPath)                                                                                      extends KPathSeq { def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue, v4: AnyValue)                                                                                      = Seq(path1 -> v1, path2 -> v2, path3 -> v3, path4 -> v4);                                                                                 override val paths: Seq[KPath] = Seq(path1, path2, path3, path4) }
  case class KPaths5 (path1: KPath, path2: KPath, path3: KPath, path4: KPath, path5: KPath)                                                                        extends KPathSeq { def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue, v4: AnyValue, v5: AnyValue)                                                                        = Seq(path1 -> v1, path2 -> v2, path3 -> v3, path4 -> v4, path5 -> v5);                                                                    override val paths: Seq[KPath] = Seq(path1, path2, path3, path4, path5) }
  case class KPaths6 (path1: KPath, path2: KPath, path3: KPath, path4: KPath, path5: KPath, path6: KPath)                                                          extends KPathSeq { def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue, v4: AnyValue, v5: AnyValue, v6: AnyValue)                                                          = Seq(path1 -> v1, path2 -> v2, path3 -> v3, path4 -> v4, path5 -> v5, path6 -> v6);                                                       override val paths: Seq[KPath] = Seq(path1, path2, path3, path4, path5, path6) }
  case class KPaths7 (path1: KPath, path2: KPath, path3: KPath, path4: KPath, path5: KPath, path6: KPath, path7: KPath)                                            extends KPathSeq { def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue, v4: AnyValue, v5: AnyValue, v6: AnyValue, v7: AnyValue)                                            = Seq(path1 -> v1, path2 -> v2, path3 -> v3, path4 -> v4, path5 -> v5, path6 -> v6, path7 -> v7);                                          override val paths: Seq[KPath] = Seq(path1, path2, path3, path4, path5, path6, path7) }
  case class KPaths8 (path1: KPath, path2: KPath, path3: KPath, path4: KPath, path5: KPath, path6: KPath, path7: KPath, path8: KPath)                              extends KPathSeq { def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue, v4: AnyValue, v5: AnyValue, v6: AnyValue, v7: AnyValue, v8: AnyValue)                              = Seq(path1 -> v1, path2 -> v2, path3 -> v3, path4 -> v4, path5 -> v5, path6 -> v6, path7 -> v7, path8 -> v8);                             override val paths: Seq[KPath] = Seq(path1, path2, path3, path4, path5, path6, path7, path8) }
  case class KPaths9 (path1: KPath, path2: KPath, path3: KPath, path4: KPath, path5: KPath, path6: KPath, path7: KPath, path8: KPath, path9: KPath)                extends KPathSeq { def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue, v4: AnyValue, v5: AnyValue, v6: AnyValue, v7: AnyValue, v8: AnyValue, v9: AnyValue)                = Seq(path1 -> v1, path2 -> v2, path3 -> v3, path4 -> v4, path5 -> v5, path6 -> v6, path7 -> v7, path8 -> v8, path9 -> v9);                override val paths: Seq[KPath] = Seq(path1, path2, path3, path4, path5, path6, path7, path8, path9) }  
  case class KPaths10(path1: KPath, path2: KPath, path3: KPath, path4: KPath, path5: KPath, path6: KPath, path7: KPath, path8: KPath, path9: KPath, path10: KPath) extends KPathSeq { def entries(v1: AnyValue, v2: AnyValue, v3: AnyValue, v4: AnyValue, v5: AnyValue, v6: AnyValue, v7: AnyValue, v8: AnyValue, v9: AnyValue, v10: AnyValue) = Seq(path1 -> v1, path2 -> v2, path3 -> v3, path4 -> v4, path5 -> v5, path6 -> v6, path7 -> v7, path8 -> v8, path9 -> v9, path10 -> v10); override val paths: Seq[KPath] = Seq(path1, path2, path3, path4, path5, path6, path7, path8, path9, path10) }

// ===========================================================================
