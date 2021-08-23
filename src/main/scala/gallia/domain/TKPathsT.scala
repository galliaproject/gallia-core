package gallia.domain

import aptus._
import gallia._
import gallia.vldt.MetaValidation

// ===========================================================================
trait TKPathSeq extends KPathSeq {
	  def values: Seq[TKPath]
	  final def paths : Seq[KPath] = values.map(_.path)

	  // ---------------------------------------------------------------------------
    override def vldtAsNewDestination(c: Cls): Errs = 
      super.vldtAsNewDestination(c) ++
      values.map(_.tipe).flatMap(MetaValidation.validType)
  }

  // ---------------------------------------------------------------------------
  case class TKPaths2(path1: TKPath, path2: TKPath) extends TKPathSeq {
	  def values = Seq(path1, path2)	  
    def kpathT: KPaths2 = paths.force.tuple2.thn(KPaths2.tupled)    
  }
  case class TKPaths3 (path1: TKPath, path2: TKPath, path3: TKPath)                                                                                                           extends TKPathSeq { def values = Seq(path1, path2, path3);                                                   def kpathT: KPaths3  = paths.force.tuple3 .thn(KPaths3 .tupled) }
  case class TKPaths4 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath)                                                                                            extends TKPathSeq { def values = Seq(path1, path2, path3, path4);                                            def kpathT: KPaths4  = paths.force.tuple4 .thn(KPaths4 .tupled) }
  case class TKPaths5 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath)                                                                             extends TKPathSeq { def values = Seq(path1, path2, path3, path4, path5);                                     def kpathT: KPaths5  = paths.force.tuple5 .thn(KPaths5 .tupled) }
  case class TKPaths6 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath, path6: TKPath)                                                              extends TKPathSeq { def values = Seq(path1, path2, path3, path4, path5, path6);                              def kpathT: KPaths6  = paths.force.tuple6 .thn(KPaths6 .tupled) }
  case class TKPaths7 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath, path6: TKPath, path7: TKPath)                                               extends TKPathSeq { def values = Seq(path1, path2, path3, path4, path5, path6, path7);                       def kpathT: KPaths7  = paths.force.tuple7 .thn(KPaths7 .tupled) }
  case class TKPaths8 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath, path6: TKPath, path7: TKPath, path8: TKPath)                                extends TKPathSeq { def values = Seq(path1, path2, path3, path4, path5, path6, path7, path8);                def kpathT: KPaths8  = paths.force.tuple8 .thn(KPaths8 .tupled) }
  case class TKPaths9 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath, path6: TKPath, path7: TKPath, path8: TKPath, path9: TKPath)                 extends TKPathSeq { def values = Seq(path1, path2, path3, path4, path5, path6, path7, path8, path9);         def kpathT: KPaths9  = paths.force.tuple9 .thn(KPaths9 .tupled) }
  case class TKPaths10(path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath, path6: TKPath, path7: TKPath, path8: TKPath, path9: TKPath, path10: TKPath) extends TKPathSeq { def values = Seq(path1, path2, path3, path4, path5, path6, path7, path8, path9, path10); def kpathT: KPaths10 = paths.force.tuple10.thn(KPaths10.tupled) }

// ===========================================================================