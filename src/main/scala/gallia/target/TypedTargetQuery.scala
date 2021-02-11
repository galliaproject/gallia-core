package gallia.target

import aptus.{Anything_, Seq_}

import gallia._
import gallia.meta._
import gallia.target.utils.TypedTargetQueryUtils
import gallia.domain._

// ===========================================================================
// term: t210201151634 - (target) replace "Query" with "Selection" throughout?
case class TypedTargetQuery[$Target]( // t210110103720 - subclass TargetQuery and TypedTargetQuery rather than using (ev <:<
        tq             : TargetQuery[$Target],
        node           : TypeNode,
        instantiator   : Instantiator,
        ignoreContainer: Boolean /* eg for stringx('foo) */)
      extends HasType
         with HasTypedTargetQuerySeq[$Target]
         with _TypedTargetQuery[$Target] {

    override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(this)

    def size(c: Cls): Int = tq.size(c)

    // ---------------------------------------------------------------------------
    def duo(c: Cls) = Duo[$Target](node, tq.resolve(c))

    def kpathT (c: Cls)(implicit ev: $Target <:< KPath ): KPath  = kpath_(c)
    def kpath_ (c: Cls)(implicit ev: $Target <:< KPath ): KPath  = tq.resolve(c)
    def qpathz_(c: Cls)(implicit ev: $Target <:< RPathz): RPathz = tq.resolve(c)

    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair =
      tq
        .pathPairT(c: Cls)
        .assert(
            _.optional == node.leaf.isOption ||
            node.isContainedWhatever /* TODO: contained ok here? */)

    def tqkpath(implicit ev: $Target <:< KPath): TqKPath = tq.asInstanceOf[TqKPath]

    def wrapx (c: Cls,                  f: Any => Any): Any => Any = tq.container(c).containerWrap(f)
    def wrapxc(c: Cls, to: HasTypeNode, f: Any => Any): Any => Any = tq.container(c).containerWrap(wrapc(to, f))

    // ===========================================================================
    // meta

    def fromOverride(value: HasType)(implicit ev: $Target <:< KPath): TtqKPath = super.ttqkpaths(value).force.one

    def forceData(c: Cls): $Target = tq.resolve(c)
  }

  // ===========================================================================
  case class TypedTargetQuery2[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target])
        extends HasTypes2 with HasTypedTargetQuerySeq[$Target] with _TypedTargetQuery[$Target] {

      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2)
      def ht1 = ttq1; def ht2 = ttq2

      // ---------------------------------------------------------------------------
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair2 = PathPair2(ttq1.pathPairT(c), ttq2.pathPairT(c))
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths2   = super.__kpathz(c).values.force.tuple2.thn(KPaths2.tupled)

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes2)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath2(super.ttqkpaths(value).force.tuple2)
    }

  // ===========================================================================
  case class TypedTargetQuery3[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target])
        extends HasTypes3 with HasTypedTargetQuerySeq[$Target] with _TypedTargetQuery[$Target] {
      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths3   = super.__kpathz(c).values.force.tuple3.thn(KPaths3.tupled)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair3 = PathPair3(ttq1.pathPairT(c), ttq2.pathPairT(c), ttq3.pathPairT(c))

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes3)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath3(super.ttqkpaths(value).force.tuple3)
    }

  // ===========================================================================
  case class TypedTargetQuery4[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target])
        extends HasTypes4 with HasTypedTargetQuerySeq[$Target] with _TypedTargetQuery[$Target] {
      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths4   = super.__kpathz(c).values.force.tuple4.thn(KPaths4.tupled)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair4 = PathPair4(ttq1.pathPairT(c), ttq2.pathPairT(c), ttq3.pathPairT(c), ttq4.pathPairT(c))

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes3)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath4(super.ttqkpaths(value).force.tuple4)
    }

  // ===========================================================================
  case class TypedTargetQuery5[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target], ttq5: TypedTargetQuery[$Target])
        extends HasTypes5 with HasTypedTargetQuerySeq[$Target] with _TypedTargetQuery[$Target] {
      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4, ttq5)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4; def ht5 = ttq5

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths5   = super.__kpathz(c).values.force.tuple5.thn(KPaths5.tupled)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair5 = PathPair5(ttq1.pathPairT(c), ttq2.pathPairT(c), ttq3.pathPairT(c), ttq4.pathPairT(c), ttq5.pathPairT(c))

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes3)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath5(super.ttqkpaths(value).force.tuple5)
    }

  // ===========================================================================
  case class TypedTargetQuery6[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target], ttq5: TypedTargetQuery[$Target], ttq6: TypedTargetQuery[$Target])
        extends HasTypes6 with HasTypedTargetQuerySeq[$Target] with _TypedTargetQuery[$Target] {
      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4, ttq5, ttq6)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4; def ht5 = ttq5; def ht6 = ttq6

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths6   = super.__kpathz(c).values.force.tuple6.thn(KPaths6.tupled)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair6 = PathPair6(ttq1.pathPairT(c), ttq2.pathPairT(c), ttq3.pathPairT(c), ttq4.pathPairT(c), ttq5.pathPairT(c), ttq6.pathPairT(c))

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes6)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath6(super.ttqkpaths(value).force.tuple6)
    }

  // ===========================================================================
  case class TypedTargetQuery7[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target], ttq5: TypedTargetQuery[$Target], ttq6: TypedTargetQuery[$Target], ttq7: TypedTargetQuery[$Target])
        extends HasTypes7 with HasTypedTargetQuerySeq[$Target] with _TypedTargetQuery[$Target] {
      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4, ttq5, ttq6, ttq7)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4; def ht5 = ttq5; def ht6 = ttq6; def ht7 = ttq7

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths7   = super.__kpathz(c).values.force.tuple7.thn(KPaths7.tupled)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair7 = PathPair7(ttq1.pathPairT(c), ttq2.pathPairT(c), ttq3.pathPairT(c), ttq4.pathPairT(c), ttq5.pathPairT(c), ttq6.pathPairT(c), ttq7.pathPairT(c))

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes7)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath7(super.ttqkpaths(value).force.tuple7)
    }

  // ===========================================================================
  case class TypedTargetQuery8[$Target](ttq1: TypedTargetQuery[$Target], ttq2: TypedTargetQuery[$Target], ttq3: TypedTargetQuery[$Target], ttq4: TypedTargetQuery[$Target], ttq5: TypedTargetQuery[$Target], ttq6: TypedTargetQuery[$Target], ttq7: TypedTargetQuery[$Target], ttq8: TypedTargetQuery[$Target])
        extends HasTypes8 with HasTypedTargetQuerySeq[$Target] with _TypedTargetQuery[$Target] {
      final override def ttqs: Seq[TypedTargetQuery[$Target]] = Seq(ttq1, ttq2, ttq3, ttq4, ttq5, ttq6, ttq7, ttq8)
      def ht1 = ttq1; def ht2 = ttq2; def ht3 = ttq3; def ht4 = ttq4; def ht5 = ttq5; def ht6 = ttq6; def ht7 = ttq7; def ht8 = ttq8

      // ---------------------------------------------------------------------------
      def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths8   = super.__kpathz(c).values.force.tuple8.thn(KPaths8.tupled)
      def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair8 = PathPair8(ttq1.pathPairT(c), ttq2.pathPairT(c), ttq3.pathPairT(c), ttq4.pathPairT(c), ttq5.pathPairT(c), ttq6.pathPairT(c), ttq7.pathPairT(c), ttq8.pathPairT(c))

      // ---------------------------------------------------------------------------
      def fromOverride(value: HasTypes8)(implicit ev: $Target <:< KPath) = TypedTargetQueryUtils.ttqkpath8(super.ttqkpaths(value).force.tuple8)
    }

// ===========================================================================
