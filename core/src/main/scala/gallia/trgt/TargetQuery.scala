package gallia
package trgt

import aptus.Anything_
import domain._
import vldt.{MetaValidation, SpecialCardiMode}
import selection.untyped.processors.RPathzSelection

// ===========================================================================
class TargetQuery[$Target /* TODO: t210823111030 - ungenerify */](
    //TODO: t210110103720 - subclass rather, in terms of $Target + pass actual selection when possible (especially for explicit)?
        val vldtTargetQuery : Cls => Errs,
        val resolve         : Cls => $Target /* 201005113232 - consistently same cardi/type by design */)
       extends Serializable /* TODO: t240108113850 - ideally shouldn't be needed, but see the likes of 240108113936 */
          with CanValidateQuery
          with CanResolve[$Target] {

    def apply[A](c: Cls)(f: $Target => A): A = resolve(c).pipe(f)

    def size(c: Cls): Int = __rpaths(c).size

    // ---------------------------------------------------------------------------
    def filterByKeys (c: Cls)(implicit ev: $Target <:< Keyz  ): Cls = resolve(c).pipe { x => c.filterFields(field => x.contains(field.key)) }
    def filterByPaths(c: Cls)(implicit ev: $Target <:< KPathz): Cls = ???

    // ---------------------------------------------------------------------------
    def info (c: Cls)(implicit ev: $Target <:< KPath) = c.field(kpath_(c)).info

    // ---------------------------------------------------------------------------
    def isRequired(c: Cls) = _is(c, _.isRequired)
    def isOptional(c: Cls) = _is(c, _.isOptional)

      private def _is(c: Cls, pred: Fld => Boolean)   : Boolean =
        __rpaths(c)
          .headOption /* since all supposed to be the same: 201005113232 */
          .map(_.from.pipe(c.field(_)))
          .exists(pred)

    // ---------------------------------------------------------------------------
    def container1(c: Cls): Container =
        __rpaths(c)
          .head /* since all supposed to be the same: 201005113232 */
          .from.pipe(c.field(_))
          .info
          .container1

    // ---------------------------------------------------------------------------
    def keyz_  (c: Cls)(implicit ev: $Target <:< Keyz  ): Keyz     = resolve(c)
    def keys_  (c: Cls)(implicit ev: $Target <:< Keyz  ): Seq[Key] = resolve(c).values.toSeq
    def kpath_ (c: Cls)(implicit ev: $Target <:< KPath ): KPath    = resolve(c)
    def kpathz_(c: Cls)(implicit ev: $Target <:< KPathz): KPathz   = resolve(c)
    def rpathz_(c: Cls)(implicit ev: $Target <:< RPathz): RPathz   = resolve(c)

    // ---------------------------------------------------------------------------
    def __kpathz(c: Cls): KPathz     = KPathz(__kpaths(c))
    def __kpaths(c: Cls): Seq[KPath] =
      resolve(c) match {
        case x: KPath  => Seq(x)
        case x: Keyz   => x.kpathz.values
        case x: Renz   => x.rpathz.fromsFX
        case x: RPathz => x.fromsFX }

    // ---------------------------------------------------------------------------
    def __rpathz(c: Cls): RPathz     = RPathz(__rpaths(c))
    def __rpaths(c: Cls): Seq[RPath] =
      resolve(c) match {
        case x: Key    => RPathz.from(x).values
        case x: Ren    => RPathz.from(x).values
        case x: KPath  => x.rpath.in.seq
        case x: Keyz   => x.rpathz.values
        case x: Renz   => x.rpathz.values
        case x: KPathz => x.rpathz.values
        case x: RPathz => x       .values }

    // ---------------------------------------------------------------------------
    def checkErrors(c: Cls)(f: Cls => KPathz => Errs): Errs = __rpathz(c).pipe(_.fromz).pipe { f(c) }

    // ===========================================================================
    // vldt

    def vldtAsOrigin        (c: Cls): Errs = vldtTargetQuery(c) ++ MetaValidation.fieldsRenaming(c, __rpaths(c).pipe(RPathz.apply))
    def vldtAsNewDestination(c: Cls): Errs = vldtTargetQuery(c) ++ MetaValidation.fieldsAbsence (c, __kpathz(c))

    // ---------------------------------------------------------------------------
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath ):  PathPair = kpath_(c).pipe { path => PathPair(path, c.isOptional(path)) }
    def path     (c: Cls)(implicit ev: $Target <:< KPath ): KPath     = kpath_(c)

    // ---------------------------------------------------------------------------
    def duo(c: Cls, node: TypeNode): Duo[$Target] = Duo(node, resolve(c))
  }

  // ===========================================================================
  object TargetQuery { // see more basic builders in TargetQueryUtils
    private def from[A](a: A)(f: A => RPathz): TqRPathz = new TqRPathz(_ => Nil, _ => f(a))

    // ---------------------------------------------------------------------------
    def fromKey(key: Key): TqRPathz = from(key)(RPathz.from)

    // ---------------------------------------------------------------------------
    implicit def toTqRPathz(x: RPathW)                       : TqRPathz = from(x)(_.rpathz)
    implicit def toTqRPathz(x: RPathWz)                      : TqRPathz = from(x)(_.rpathz)
    implicit def toTqRPathz(x: (RPathW, RPathW, Seq[RPathW])): TqRPathz = toTqRPathz(RPathWz(Seq(x._1, x._2) ++ x._3))

    implicit def toTqRPathz(f: RPathzSelection): TqRPathz = new TqRPathz(f.vldt, f.rpathz)
}

// ===========================================================================
case class TargetQuery2[$Target](tq1: TQ[$Target], tq2: TQ[$Target]) extends HasTargetQuerySeq[$Target] {
    override def tqs = Seq(tq1, tq2)
    
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair2 = PathPair2(tq1.pathPairT(c), tq2.pathPairT(c))  
    def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths2   = KPaths2  (tq1.path(c),      tq2.path(c))
  }
  
  // ---------------------------------------------------------------------------
  case class TargetQuery3[$Target](tq1: TQ[$Target], tq2: TQ[$Target], tq3: TQ[$Target]) extends HasTargetQuerySeq[$Target] {
    override def tqs = Seq(tq1, tq2, tq3)
    
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair3 = PathPair3(tq1.pathPairT(c), tq2.pathPairT(c), tq3.pathPairT(c))  
    def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths3   = KPaths3  (tq1.path(c),      tq2.path(c),      tq3.path(c))
  }

  // ---------------------------------------------------------------------------
  case class TargetQuery4[$Target](tq1: TQ[$Target], tq2: TQ[$Target], tq3: TQ[$Target], tq4: TQ[$Target]) extends HasTargetQuerySeq[$Target] {
    override def tqs = Seq(tq1, tq2, tq3, tq4)
    
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair4 = PathPair4(tq1.pathPairT(c), tq2.pathPairT(c), tq3.pathPairT(c), tq4.pathPairT(c))  
    def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths4   = KPaths4  (tq1.path(c),      tq2.path(c),      tq3.path(c),      tq4.path(c))
  }
  
  // ---------------------------------------------------------------------------
  case class TargetQuery5[$Target](tq1: TQ[$Target], tq2: TQ[$Target], tq3: TQ[$Target], tq4: TQ[$Target], tq5: TQ[$Target]) extends HasTargetQuerySeq[$Target] {
    override def tqs = Seq(tq1, tq2, tq3, tq4, tq5)
    
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair5 = PathPair5(tq1.pathPairT(c), tq2.pathPairT(c), tq3.pathPairT(c), tq4.pathPairT(c), tq5.pathPairT(c))  
    def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths5   = KPaths5  (tq1.path(c),      tq2.path(c),      tq3.path(c),      tq4.path(c),      tq5.path(c))
  }
  
  // ---------------------------------------------------------------------------
  case class TargetQuery6[$Target](tq1: TQ[$Target], tq2: TQ[$Target], tq3: TQ[$Target], tq4: TQ[$Target], tq5: TQ[$Target], tq6: TQ[$Target]) extends HasTargetQuerySeq[$Target] {
    override def tqs = Seq(tq1, tq2, tq3, tq4, tq5, tq6)
    
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair6 = PathPair6(tq1.pathPairT(c), tq2.pathPairT(c), tq3.pathPairT(c), tq4.pathPairT(c), tq5.pathPairT(c), tq6.pathPairT(c))  
    def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths6   = KPaths6  (tq1.path(c),      tq2.path(c),      tq3.path(c),      tq4.path(c),      tq5.path(c),      tq6.path(c))
  }
  
  // ---------------------------------------------------------------------------
  case class TargetQuery7[$Target](tq1: TQ[$Target], tq2: TQ[$Target], tq3: TQ[$Target], tq4: TQ[$Target], tq5: TQ[$Target], tq6: TQ[$Target], tq7: TQ[$Target]) extends HasTargetQuerySeq[$Target] {
    override def tqs = Seq(tq1, tq2, tq3, tq4, tq5, tq6, tq7)
    
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair7 = PathPair7(tq1.pathPairT(c), tq2.pathPairT(c), tq3.pathPairT(c), tq4.pathPairT(c), tq5.pathPairT(c), tq6.pathPairT(c), tq7.pathPairT(c))  
    def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths7   = KPaths7  (tq1.path(c),      tq2.path(c),      tq3.path(c),      tq4.path(c),      tq5.path(c),      tq6.path(c),      tq7.path(c))
  }
  
  // ---------------------------------------------------------------------------
  case class TargetQuery8[$Target](tq1: TQ[$Target], tq2: TQ[$Target], tq3: TQ[$Target], tq4: TQ[$Target], tq5: TQ[$Target], tq6: TQ[$Target], tq7: TQ[$Target], tq8: TQ[$Target]) extends HasTargetQuerySeq[$Target] {
    override def tqs = Seq(tq1, tq2, tq3, tq4, tq5, tq6, tq7, tq8)
    
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair8 = PathPair8(tq1.pathPairT(c), tq2.pathPairT(c), tq3.pathPairT(c), tq4.pathPairT(c), tq5.pathPairT(c), tq6.pathPairT(c), tq7.pathPairT(c), tq8.pathPairT(c))  
    def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths8   = KPaths8  (tq1.path(c),      tq2.path(c),      tq3.path(c),      tq4.path(c),      tq5.path(c),      tq6.path(c),      tq7.path(c),      tq8.path(c))
  }
  
  // ---------------------------------------------------------------------------
  case class TargetQuery9[$Target](tq1: TQ[$Target], tq2: TQ[$Target], tq3: TQ[$Target], tq4: TQ[$Target], tq5: TQ[$Target], tq6: TQ[$Target], tq7: TQ[$Target], tq8: TQ[$Target], tq9: TQ[$Target]) extends HasTargetQuerySeq[$Target] {
    override def tqs = Seq(tq1, tq2, tq3, tq4, tq5, tq6, tq7, tq8, tq9)
    
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair9 = PathPair9(tq1.pathPairT(c), tq2.pathPairT(c), tq3.pathPairT(c), tq4.pathPairT(c), tq5.pathPairT(c), tq6.pathPairT(c), tq7.pathPairT(c), tq8.pathPairT(c), tq9.pathPairT(c))  
    def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths9   = KPaths9  (tq1.path(c),      tq2.path(c),      tq3.path(c),      tq4.path(c),      tq5.path(c),      tq6.path(c),      tq7.path(c),      tq8.path(c),      tq9.path(c))
  }
  
  // ---------------------------------------------------------------------------
  case class TargetQuery10[$Target](tq1: TQ[$Target], tq2: TQ[$Target], tq3: TQ[$Target], tq4: TQ[$Target], tq5: TQ[$Target], tq6: TQ[$Target], tq7: TQ[$Target], tq8: TQ[$Target], tq9: TQ[$Target], tq10: TQ[$Target]) extends HasTargetQuerySeq[$Target] {
    override def tqs = Seq(tq1, tq2, tq3, tq4, tq5, tq6, tq7, tq8, tq9, tq10)
    
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair10 = PathPair10(tq1.pathPairT(c), tq2.pathPairT(c), tq3.pathPairT(c), tq4.pathPairT(c), tq5.pathPairT(c), tq6.pathPairT(c), tq7.pathPairT(c), tq8.pathPairT(c), tq9.pathPairT(c), tq10.pathPairT(c))  
    def kpathT   (c: Cls)(implicit ev: $Target <:< KPath): KPaths10   = KPaths10  (tq1.path(c),      tq2.path(c),      tq3.path(c),      tq4.path(c),      tq5.path(c),      tq6.path(c),      tq7.path(c),      tq8.path(c),      tq9.path(c),      tq10.path(c))
  }

// ===========================================================================
