package gallia.target

import aptus.Anything_

import gallia._
import gallia.domain._
import gallia.selection.untyped.processors.RPathzSelection
import gallia.vldt.MetaValidation

// ===========================================================================
class TargetQuery[$Target]( //TODO: t210110103720 - subclass rather, in terms of $Target + pass actual selection when possible (especially for explicit)?
        val vldtTargetQuery : Cls => Errs,
        val resolve         : Cls => $Target /* 201005113232 - consistently same cardi/type by design */)
       extends CanValidateQuery
          with CanResolve[$Target] {

    def apply[A](c: Cls)(f: $Target => A): A = resolve(c).thn(f)

    def size(c: Cls): Int = __qpaths(c).size

    // ---------------------------------------------------------------------------
    def filterByKeys (c: Cls)(implicit ev: $Target <:< Keyz  ): Cls = resolve(c).thn { x => c.filterFields(field => x.contains(field.key)) }
    def filterByPaths(c: Cls)(implicit ev: $Target <:< KPathz): Cls = ???

    // ---------------------------------------------------------------------------
    def isMultiple(c: Cls) = _is(c, _.isMultiple)
    def isRequired(c: Cls) = _is(c, _.isRequired)
    def isOptional(c: Cls) = _is(c, _.isOptional)

      private def _is(c: Cls, pred: Fld => Boolean)   : Boolean =
        __qpaths(c)
          .headOption /* since all supposed to be the same: 201005113232 */
          .map(_.from.thn(c.field(_)))
          .exists(pred)

    // ---------------------------------------------------------------------------
    def container(c: Cls): Container =
        __qpaths(c)
          .head /* since all supposed to be the same: 201005113232 */
          .from.thn(c.field(_))
          .info.container

    // ---------------------------------------------------------------------------
    def keyz_  (c: Cls)(implicit ev: $Target <:< Keyz  ): Keyz     = resolve(c)
    def keys_  (c: Cls)(implicit ev: $Target <:< Keyz  ): Seq[Key] = resolve(c).values.toSeq
    def kpath_ (c: Cls)(implicit ev: $Target <:< KPath ): KPath    = resolve(c)
    def qpathz_(c: Cls)(implicit ev: $Target <:< RPathz): RPathz   = resolve(c)

    // ---------------------------------------------------------------------------
    def __kpathz(c: Cls): KPathz     = KPathz(__kpaths(c))
    def __kpaths(c: Cls): Seq[KPath] =
      resolve(c) match {
        case x: KPath  => Seq(x)
        case x: Keyz   => x.kpathz.values
        case x: Renz   => x.qpathz.fromsFX
        case x: RPathz => x.fromsFX }

    // ---------------------------------------------------------------------------
    def __qpathz(c: Cls): RPathz     = RPathz(__qpaths(c))
    def __qpaths(c: Cls): Seq[RPath] =
      resolve(c) match {
        case x: KPath  => x.qpath.as.seq
        case x: Keyz   => x.qpathz.values
        case x: Renz   => x.qpathz.values
        case x: RPathz => x.values }

    // ---------------------------------------------------------------------------
    def checkErrors(c: Cls)(f: Cls => KPathz => Errs): Errs = __qpathz(c).thn(_.fromz).thn { f(c) }

    // ===========================================================================
    // vldt

    def vldtAsOrigin(c: Cls): Errs =
      vldtTargetQuery(c) ++
      MetaValidation.fieldsRenaming(c, __qpaths(c).thn(RPathz.apply))

    // ---------------------------------------------------------------------------
    def pathPairT(c: Cls)(implicit ev: $Target <:< KPath ): PathPair = kpath_(c).thn { path => PathPair(path, c.isOptional(path)) }
  }

  // ===========================================================================
  object TargetQuery {
    private def from[A](a: A)(f: A => RPathz): TqRPathz = new TqRPathz(_ => Nil, _ => f(a))

    // ---------------------------------------------------------------------------
    implicit def toTqRPathz(x: RPathW)                       : TqRPathz = from(x)(_.qpathz)
    implicit def toTqRPathz(x: RPathWz)                      : TqRPathz = from(x)(_.qpathz)
    implicit def toTqRPathz(x: (RPathW, RPathW, Seq[RPathW])): TqRPathz = toTqRPathz(RPathWz(Seq(x._1, x._2) ++ x._3))

    implicit def toTqRPathz(f: RPathzSelection): TqRPathz = new TqRPathz(f.vldt, f.qpathz)
}

// ===========================================================================
case class TargetQuery2[$Target](tq1: TargetQuery[$Target], tq2: TargetQuery[$Target]) extends HasTargetQuerySeq[$Target] {
  def vldtAsOrigin(c: Cls): Errs = tq1.vldtAsOrigin(c) ++ tq2.vldtAsOrigin(c) // TODO: also validate distinct

  // ---------------------------------------------------------------------------  
  def pathPairT(c: Cls)(implicit ev: $Target <:< KPath): PathPair2 = PathPair2(tq1.pathPairT(c), tq2.pathPairT(c))

  override def tqs: Seq[TargetQuery[$Target]] = Seq(tq1, tq2)
}

// ===========================================================================
