package gallia.target

import aptus.{Anything_, Seq_}

import gallia._
import gallia.target.utils.TypedTargetQueryUtils
import gallia.vldt.MetaValidation
import gallia.vldt.SpecialCardiMode
import gallia.meta.Containee

// ===========================================================================
case class Duo[$Target](node: TypeNode, target: $Target) extends HasTypeNode // TODO: rename

// ===========================================================================
trait CanResolve[$Target] { val resolve: Cls => $Target } // for TargetQuery

// ---------------------------------------------------------------------------
trait CanValidateQuery { val vldtTargetQuery : Cls => Errs    }

// ---------------------------------------------------------------------------
trait HasTypeSeq { def hts: Seq[HasType] }

// ===========================================================================
trait CanValidateQueries { protected /* use do */ def vldtTargetQueries: Seq[CanValidateQuery] }

  // ===========================================================================
  trait HasTargetQuerySeq[$Target] extends CanValidateQueries {
            def tqs                                     : Seq[TargetQuery[$Target]]
      final def tqkpaths(implicit ev: $Target <:< KPath): Seq[TqKPath] = tqs.map(_.asInstanceOf[TqKPath])

      // ---------------------------------------------------------------------------
      def targets(c: Cls): Seq[$Target] = tqs.map(_.resolve(c))

      // ---------------------------------------------------------------------------
      def pathz(c: Cls)(implicit ev: $Target <:< KPath) = KPathz(targets(c).asInstanceOf[Seq[KPath]])

      // ---------------------------------------------------------------------------
      final def doVldtTargetQueries(c: Cls): Errs = vldtTargetQueries.flatMap(_.vldtTargetQuery(c))

      // ---------------------------------------------------------------------------
      protected // use do
        final override def vldtTargetQueries: Seq[CanValidateQuery] = tqs
    }

    // ===========================================================================
    trait HasTypedTargetQuerySeq[$Target] extends HasTargetQuerySeq[$Target] {
      def ttqs: Seq[TypedTargetQuery[$Target]]

      // ---------------------------------------------------------------------------
      final override def tqs: Seq[TargetQuery[$Target]] = ttqs.map(_.tq)

      // ---------------------------------------------------------------------------
      def __kpathz(c: Cls): KPathz = ttqs.flatMap(_.tq.__kpaths(c)).thn(KPathz.apply)
      def __qpathz(c: Cls): RPathz = ttqs.flatMap(_.tq.__qpaths(c)).thn(RPathz.apply)

      final def duos(c: Cls): Seq[Duo[$Target]] = ttqs.map(_.duo(c))

      // ---------------------------------------------------------------------------
      // vldt

      // TODO: t210811103604 - move the fieldsRenaming+typeCompatibilities parts of these individuals  
      def vldtAsOrigin(c: Cls): Errs = vldtAsOrigin(c, mode = SpecialCardiMode.Normal)
      def vldtAsOrigin(c: Cls, mode: SpecialCardiMode): Errs =
        super.doVldtTargetQueries(c) ++
        MetaValidation.fieldsRenaming     (c, __qpathz(c)) ++
        MetaValidation.typeCompatibilities(c, duos(c), mode) ++
        MetaValidation.distinctRPathz     (   __qpathz(c))

      // ---------------------------------------------------------------------------
      // meta

      def containees(c:Cls)(implicit ev: $Target <:< KPath): Seq[Containee] = tqs.map(_.kpath_(c)).map(c.field(_).info.containee)

      // ---------------------------------------------------------------------------
      @deprecated("see 210111095156") def puts0(c: Cls, froms: Seq[Containee] /* distinct */)(implicit ev: $Target <:< KPath): Cls =
          ttqs
            .map { ttq =>
              val path = ttq.kpath_(c)
              path -> {
                val n = ttq.node

                if (n.isContainedWhatever) _info(ttq, froms.distinct.force.one /* see 210111091636 */)
                else                       n.forceNonBObjInfo } }
            .foldLeft(c)(_ put _)

        // ---------------------------------------------------------------------------
        def puts1(c: Cls)(implicit ev: $Target <:< KPath): Cls =
          ttqs
            .map { ttq =>
              ttq.kpath_(c) ->
                ttq.node.forceNonBObjInfo }
            .foldLeft(c)(_ put _)

        // ---------------------------------------------------------------------------
        def puts2(c: Cls, soleContainee: Containee)(implicit ev: $Target <:< KPath): Cls =
          ttqs
            .map { ttq =>
              ttq.kpath_(c) ->
                _info(ttq, soleContainee) }
            .foldLeft(c)(_ put _)

          // ===========================================================================
          private def _info(ttq: TypedTargetQuery[$Target], soleContainee: Containee)(implicit ev: $Target <:< KPath) =
            gallia.meta.Info(
              ttq.node.containerType,
              soleContainee )
    }

// ===========================================================================
trait HasTtqKPaths[$Target] { self: HasTypeSeq with HasTargetQuerySeq[$Target] =>

  def ttqkpaths(value: HasTypeSeq)(implicit ev: $Target <:< KPath): Seq[TtqKPath] =
    tqkpaths.zip(value.hts).map(x => TypedTargetQueryUtils.ttqkpath1(x._1, x._2))

}

// ===========================================================================
// TODO: rename...
trait _TypedTargetQuery[$Target] extends HasTtqKPaths[$Target] { self: HasTypeSeq with HasTypedTargetQuerySeq[$Target] => // only need hts and __kpathz...

  def vldtAsCotransformDestination(c: Cls, from: KPathz): Errs =
    self.hts.thn(MetaValidation.validTypes(c, _)) ++
    self.__kpathz(c).values // only authorize overwrite of origins
      .filterNot(from.values.contains)
      .flatMap { kpath =>
        MetaValidation.fieldAbsence(c, kpath) }

}

// ===========================================================================
