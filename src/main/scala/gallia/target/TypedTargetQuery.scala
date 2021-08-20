package gallia.target

import aptus.{Anything_, Seq_}

import gallia._
import gallia.meta._
import gallia.target.utils.TypedTargetQueryUtils
import gallia.domain._
import gallia.vldt.MetaValidation
import gallia.vldt.SpecialCardiMode

// ===========================================================================
// term: t210201151634 - (target) replace "Query" with "Selection" throughout?
case class TypedTargetQuery[$Target]( // t210110103720 - subclass TargetQuery and TypedTargetQuery rather than using (ev <:<
        tq             : TargetQuery[$Target],
        node           : TypeNode,
        instantiator   : Instantiator,
        ignoreContainer: Boolean /* eg for stringx('foo) */)
      extends HasType { private val self = this

    def size(c: Cls): Int = tq.size(c)

    // ---------------------------------------------------------------------------
    def duo      (c: Cls): Duo[$Target]    = Duo[$Target](node, tq.resolve(c))
    def fieldPair(c: Cls): ($Target, Info) = (tq.resolve(c), node.forceNonBObjInfo)
    
    def containee(c: Cls)(implicit ev: $Target <:< KPath) = kpathT(c).thn(c.field(_).info.containee)

    def kpathT (c: Cls)(implicit ev: $Target <:< KPath ): KPath  = kpath_(c)
    def kpath_ (c: Cls)(implicit ev: $Target <:< KPath ): KPath  = tq.resolve(c)
    def qpathz_(c: Cls)(implicit ev: $Target <:< RPathz): RPathz = tq.resolve(c)

    def tqkpath(implicit ev: $Target <:< KPath): TqKPath = tq.asInstanceOf[TqKPath]
      
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair =
      tq
        .pathPairT(c: Cls)
        .assert(
            _.optional == node.leaf.isOption ||
            node.isContainedWhatever /* TODO: contained ok here? */)

    // ---------------------------------------------------------------------------
    def wrapx (c: Cls,                  f: Any => Any): Any => Any = tq.container(c).containerWrap(f)
    def wrapxc(c: Cls, to: HasTypeNode, f: Any => Any): Any => Any = tq.container(c).containerWrap(wrapc(to, f))

    // ===========================================================================
    // vldt

    def vldtAsOrigin(c: Cls)                        : Errs = vldtAsOrigin(c, mode = SpecialCardiMode.Normal)
    def vldtAsOrigin(c: Cls, mode: SpecialCardiMode): Errs = tq.vldtAsOrigin(c) ++ MetaValidation.typeCompatibility(c, duo(c), mode)

    // ---------------------------------------------------------------------------
    def vldtAsNewDestination(c: Cls): Errs = tq.vldtAsNewDestination(c) ++ MetaValidation.validType(node)
    def vldtAsAnyDestination(c: Cls): Errs =                               MetaValidation.validType(node)

    // ---------------------------------------------------------------------------
    def vldtAsCotransformDestination(c: Cls, from: KPath) (implicit ev: $Target <:< gallia.KPath): Errs = vldtAsCotransformDestination(c, KPathz(Seq(from)))
    def vldtAsCotransformDestination(c: Cls, from: KPathz)(implicit ev: $Target <:< gallia.KPath): Errs =                
      MetaValidation.validType(c, this) ++
      tq.kpath_(c).in.noneIf(from.values.contains).flatMap(MetaValidation.fieldAbsence(c, _)) // only authorize overwrite of origins

    // ===========================================================================
    // meta

    def fromOverride(value: HasType)(implicit ev: $Target <:< KPath): TtqKPath = TypedTargetQueryUtils.ttqkpath1(tqkpath, this)

    def forceData(c: Cls): $Target = tq.resolve(c)
    
    // ===========================================================================
    // temporarily
    private def asMultiple = new HasTypedTargetQuerySeq[$Target] { override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(self) }
      @deprecated("see 210111095156") def puts0(c: Cls, from:  Containee)                    (implicit ev: $Target <:< KPath): Cls = puts0(c, Seq(from))
      @deprecated("see 210111095156") def puts0(c: Cls, froms: Seq[Containee] /* distinct */)(implicit ev: $Target <:< KPath): Cls = asMultiple.puts0(c, froms)
                                      def puts1(c: Cls)                                      (implicit ev: $Target <:< KPath): Cls = asMultiple.puts1(c)
  }

  // ===========================================================================
  case class TypedTargetQuery2[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target])
        extends HasTypes2 with HasTypedTargetQuerySeq[$Target] {

      def tqkpath2(implicit ev: $Target <:< KPath) = new TargetQuery2[$Target](ttq1.tq, ttq2.tq)

      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2)
      def ht1 = ttq1; def ht2 = ttq2

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths2   = tqkpath2.kpathT(c)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair2 = tqkpath2.pathPairT(c)

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes2)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath2(ttqkpaths(value).force.tuple2)
    }

  // ===========================================================================
  case class TypedTargetQuery3[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target])
        extends HasTypes3 with HasTypedTargetQuerySeq[$Target] {
    
      def tqkpath3(implicit ev: $Target <:< KPath) = new TargetQuery3[$Target](ttq1.tq, ttq2.tq, ttq3.tq)

      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths3   = tqkpath3.kpathT(c)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair3 = tqkpath3.pathPairT(c)

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes3)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath3(ttqkpaths(value).force.tuple3)     
    }

  // ===========================================================================
  case class TypedTargetQuery4[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target])
        extends HasTypes4 with HasTypedTargetQuerySeq[$Target] {
    
      def tqkpath4(implicit ev: $Target <:< KPath) = new TargetQuery4[$Target](ttq1.tq, ttq2.tq, ttq3.tq, ttq4.tq)

      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths4   = tqkpath4.kpathT(c)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair4 = tqkpath4.pathPairT(c)

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes3)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath4(ttqkpaths(value).force.tuple4)
    }

  // ===========================================================================
  case class TypedTargetQuery5[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target], ttq5: TypedTargetQuery[$Target])
        extends HasTypes5 with HasTypedTargetQuerySeq[$Target] {
    
      def tqkpath5(implicit ev: $Target <:< KPath) = new TargetQuery5[$Target](ttq1.tq, ttq2.tq, ttq3.tq, ttq4.tq, ttq5.tq)

      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4, ttq5)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4; def ht5 = ttq5

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths5   = tqkpath5.kpathT(c)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair5 = tqkpath5.pathPairT(c)

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes3)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath5(ttqkpaths(value).force.tuple5)
    }

  // ===========================================================================
  case class TypedTargetQuery6[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target], ttq5: TypedTargetQuery[$Target], ttq6: TypedTargetQuery[$Target])
        extends HasTypes6 with HasTypedTargetQuerySeq[$Target] {
    
      def tqkpath6(implicit ev: $Target <:< KPath) = new TargetQuery6[$Target](ttq1.tq, ttq2.tq, ttq3.tq, ttq4.tq, ttq5.tq, ttq6.tq)

      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4, ttq5, ttq6)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4; def ht5 = ttq5; def ht6 = ttq6

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths6   = tqkpath6.kpathT(c)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair6 = tqkpath6.pathPairT(c)

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes6)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath6(ttqkpaths(value).force.tuple6)
    }

  // ===========================================================================
  case class TypedTargetQuery7[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target], ttq5: TypedTargetQuery[$Target], ttq6: TypedTargetQuery[$Target], ttq7: TypedTargetQuery[$Target])
        extends HasTypes7 with HasTypedTargetQuerySeq[$Target] {
    
      def tqkpath7(implicit ev: $Target <:< KPath) = new TargetQuery7[$Target](ttq1.tq, ttq2.tq, ttq3.tq, ttq4.tq, ttq5.tq, ttq6.tq, ttq7.tq)

      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4, ttq5, ttq6, ttq7)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4; def ht5 = ttq5; def ht6 = ttq6; def ht7 = ttq7

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths7   = tqkpath7.kpathT(c)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair7 = tqkpath7.pathPairT(c)

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes7)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath7(ttqkpaths(value).force.tuple7)
    }

  // ===========================================================================
  case class TypedTargetQuery8[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target], ttq5: TypedTargetQuery[$Target], ttq6: TypedTargetQuery[$Target], ttq7: TypedTargetQuery[$Target], ttq8: TypedTargetQuery[$Target])
        extends HasTypes8 with HasTypedTargetQuerySeq[$Target] {
    
      def tqkpath8(implicit ev: $Target <:< KPath) = new TargetQuery8[$Target](ttq1.tq, ttq2.tq, ttq3.tq, ttq4.tq, ttq5.tq, ttq6.tq, ttq7.tq, ttq8.tq)

      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4, ttq5, ttq6, ttq7, ttq8)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4; def ht5 = ttq5; def ht6 = ttq6; def ht7 = ttq7; def ht8 = ttq8

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths8   = tqkpath8.kpathT(c)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair8 = tqkpath8.pathPairT(c)

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes8)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath8(ttqkpaths(value).force.tuple8)
    }

  // ===========================================================================
  case class TypedTargetQuery9[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target], ttq5: TypedTargetQuery[$Target], ttq6: TypedTargetQuery[$Target], ttq7: TypedTargetQuery[$Target], ttq8: TypedTargetQuery[$Target], ttq9: TypedTargetQuery[$Target])
        extends HasTypes9 with HasTypedTargetQuerySeq[$Target] {
    
      def tqkpath9(implicit ev: $Target <:< KPath) = new TargetQuery9[$Target](ttq1.tq, ttq2.tq, ttq3.tq, ttq4.tq, ttq5.tq, ttq6.tq, ttq7.tq, ttq8.tq, ttq9.tq)

      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4, ttq5, ttq6, ttq7, ttq8, ttq9)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4; def ht5 = ttq5; def ht6 = ttq6; def ht7 = ttq7; def ht8 = ttq8; def ht9 = ttq9

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths9   = tqkpath9.kpathT(c)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair9 = tqkpath9.pathPairT(c)

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes9)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath9(ttqkpaths(value).force.tuple9)
    }

  // ===========================================================================
  case class TypedTargetQuery10[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target], ttq5: TypedTargetQuery[$Target], ttq6: TypedTargetQuery[$Target], ttq7: TypedTargetQuery[$Target], ttq8: TypedTargetQuery[$Target], ttq9: TypedTargetQuery[$Target], ttq10: TypedTargetQuery[$Target])
        extends HasTypes10 with HasTypedTargetQuerySeq[$Target] {
    
      def tqkpath10(implicit ev: $Target <:< KPath) = new TargetQuery10[$Target](ttq1.tq, ttq2.tq, ttq3.tq, ttq4.tq, ttq5.tq, ttq6.tq, ttq7.tq, ttq8.tq, ttq9.tq, ttq10.tq)

      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4, ttq5, ttq6, ttq7, ttq8, ttq9, ttq10)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4; def ht5 = ttq5; def ht6 = ttq6; def ht7 = ttq7; def ht8 = ttq8; def ht9 = ttq9; def ht10 = ttq10

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths10   = tqkpath10.kpathT(c)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair10 = tqkpath10.pathPairT(c)

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes10)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpathA(ttqkpaths(value).force.tuple10)
    }
  
// ===========================================================================
