package gallia.domain

import aptus._
import gallia._
import gallia.target._
import gallia.vldt.MetaValidation

// ===========================================================================
trait TKPathSeq extends KPathSeq {
	  def values: Seq[TKPath]
	  final def paths : Seq[KPath] = values.map(_.path)

    // ---------------------------------------------------------------------------
    // vldt
    override def vldtAsNewDestination(c: Cls): Errs = 
      super.vldtAsNewDestination(c) ++
      values.map(_.tipe).flatMap(MetaValidation.validType)
      
    // ---------------------------------------------------------------------------
    // meta
    def addAll(c: Cls) = values.map(_.fieldPair(c)).foldLeft(c)(_ add _)      
  }

  // ===========================================================================
  case class TKPaths2(path1: TKPath, path2: TKPath) extends TKPathSeq with HasTypes2 {
    val ht1 = path1; val ht2 = path2
	  def values = Seq(path1, path2)	  
    def kpathT: KPaths2 = paths.force.tuple2.pipe(KPaths2.tupled)    
  }
  
  // ---------------------------------------------------------------------------
  case class TKPaths3 (path1: TKPath, path2: TKPath, path3: TKPath)                                                                                                           extends TKPathSeq with HasTypes3  { val ht1 = path1; val ht2 = path2; val ht3 = path3;                                                                                                                          def values = Seq(path1, path2, path3);                                                   def kpathT: KPaths3  = paths.force.tuple3 .pipe(KPaths3 .tupled) }
  case class TKPaths4 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath)                                                                                            extends TKPathSeq with HasTypes4  { val ht1 = path1; val ht2 = path2; val ht3 = path3; val ht4 = path4;                                                                                                         def values = Seq(path1, path2, path3, path4);                                            def kpathT: KPaths4  = paths.force.tuple4 .pipe(KPaths4 .tupled) }
  case class TKPaths5 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath)                                                                             extends TKPathSeq with HasTypes5  { val ht1 = path1; val ht2 = path2; val ht3 = path3; val ht4 = path4; val ht5 = path5;                                                                                        def values = Seq(path1, path2, path3, path4, path5);                                     def kpathT: KPaths5  = paths.force.tuple5 .pipe(KPaths5 .tupled) }
  case class TKPaths6 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath, path6: TKPath)                                                              extends TKPathSeq with HasTypes6  { val ht1 = path1; val ht2 = path2; val ht3 = path3; val ht4 = path4; val ht5 = path5; val ht6 = path6;                                                                       def values = Seq(path1, path2, path3, path4, path5, path6);                              def kpathT: KPaths6  = paths.force.tuple6 .pipe(KPaths6 .tupled) }
  case class TKPaths7 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath, path6: TKPath, path7: TKPath)                                               extends TKPathSeq with HasTypes7  { val ht1 = path1; val ht2 = path2; val ht3 = path3; val ht4 = path4; val ht5 = path5; val ht6 = path6; val ht7 = path7;                                                      def values = Seq(path1, path2, path3, path4, path5, path6, path7);                       def kpathT: KPaths7  = paths.force.tuple7 .pipe(KPaths7 .tupled) }
  case class TKPaths8 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath, path6: TKPath, path7: TKPath, path8: TKPath)                                extends TKPathSeq with HasTypes8  { val ht1 = path1; val ht2 = path2; val ht3 = path3; val ht4 = path4; val ht5 = path5; val ht6 = path6; val ht7 = path7; val ht8 = path8;                                     def values = Seq(path1, path2, path3, path4, path5, path6, path7, path8);                def kpathT: KPaths8  = paths.force.tuple8 .pipe(KPaths8 .tupled) }
  case class TKPaths9 (path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath, path6: TKPath, path7: TKPath, path8: TKPath, path9: TKPath)                 extends TKPathSeq with HasTypes9  { val ht1 = path1; val ht2 = path2; val ht3 = path3; val ht4 = path4; val ht5 = path5; val ht6 = path6; val ht7 = path7; val ht8 = path8; val ht9 = path9;                    def values = Seq(path1, path2, path3, path4, path5, path6, path7, path8, path9);         def kpathT: KPaths9  = paths.force.tuple9 .pipe(KPaths9 .tupled) }
  case class TKPaths10(path1: TKPath, path2: TKPath, path3: TKPath, path4: TKPath, path5: TKPath, path6: TKPath, path7: TKPath, path8: TKPath, path9: TKPath, path10: TKPath) extends TKPathSeq with HasTypes10 { val ht1 = path1; val ht2 = path2; val ht3 = path3; val ht4 = path4; val ht5 = path5; val ht6 = path6; val ht7 = path7; val ht8 = path8; val ht9 = path9; val ht10 = path10; def values = Seq(path1, path2, path3, path4, path5, path6, path7, path8, path9, path10); def kpathT: KPaths10 = paths.force.tuple10.pipe(KPaths10.tupled) }

// ===========================================================================
