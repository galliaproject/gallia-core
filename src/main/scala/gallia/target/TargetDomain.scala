package gallia.target

import aptus.{Anything_, Seq_}

import gallia._
import gallia.target.utils.TypedTargetQueryUtils
import gallia.vldt.MetaValidation
import gallia.vldt.SpecialCardiMode
import gallia.meta.Containee

// ===========================================================================
  trait HasTargetQuerySeq[$Target] {
      def tqs: Seq[TargetQuery[$Target]]

      final def  tqkpaths                   (implicit ev: $Target <:< KPath): Seq[TqKPath]  = tqs                    .map(_.asInstanceOf[TqKPath])
      final def ttqkpaths(value: HasTypeSeq)(implicit ev: $Target <:< KPath): Seq[TtqKPath] = tqkpaths.zip(value.hts).map(x => TypedTargetQueryUtils.ttqkpath1(x._1, x._2))

      // ---------------------------------------------------------------------------
      def targets(c: Cls): Seq[$Target] = tqs.map(_.resolve(c))
      
      // TODO: should be private
      /*private[target] */def __kpathz(c: Cls): KPathz = tqs.flatMap(_.__kpaths(c)).thn(KPathz.apply)
      /*private[target] */def __qpathz(c: Cls): RPathz = tqs.flatMap(_.__qpaths(c)).thn(RPathz.apply)
      
      // ---------------------------------------------------------------------------
      def pathz(c: Cls)(implicit ev: $Target <:< KPath): KPathz = KPathz(targets(c).asInstanceOf[Seq[KPath]])

      // ===========================================================================
      // vldt
  
      private[target] def _vldtAsOrigin(c: Cls): Errs =
          tqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsOrigin(c)) ++ 
          MetaValidation.distinctRPathz(__qpathz(c))
  
      private[target] def _vldtAsNewDestination(c: Cls): Errs =
        tqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsNewDestination(c)) ++ 
        MetaValidation.distinctRPathz(__qpathz(c))
    }

    // ===========================================================================
    trait HasTypedTargetQuerySeq[$Target] extends HasTargetQuerySeq[$Target] {
      def ttqs: Seq[TypedTargetQuery[$Target]]

      // ---------------------------------------------------------------------------
      final override def tqs: Seq[TargetQuery[$Target]] = ttqs.map(_.tq)

      // ===========================================================================
      // vldt
  
      def vldtAsOrigin        (c: Cls)                        : Errs = vldtAsOrigin(c, mode = SpecialCardiMode.Normal)
      def vldtAsOrigin        (c: Cls, mode: SpecialCardiMode): Errs = ttqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsOrigin(c))         ++ MetaValidation.distinctRPathz(__qpathz(c))      
      def vldtAsNewDestination(c: Cls)                        : Errs = ttqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsNewDestination(c)) ++ MetaValidation.distinctRPathz(__qpathz(c))
      def vldtAsAnyDestination(c: Cls)                        : Errs = ttqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsAnyDestination(c)) ++ MetaValidation.distinctRPathz(__qpathz(c))

      // ---------------------------------------------------------------------------
      def vldtAsCotransformDestination(c: Cls, from: KPath) (implicit ev: $Target <:< gallia.KPath): Errs = vldtAsCotransformDestination(c, KPathz(Seq(from)))        
      def vldtAsCotransformDestination(c: Cls, from: KPathz)(implicit ev: $Target <:< gallia.KPath): Errs =
        ttqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsCotransformDestination(c, from)) ++ MetaValidation.distinctRPathz(__qpathz(c))

      // ===========================================================================
      // meta

      def containees(c:Cls)(implicit ev: $Target <:< KPath): Seq[Containee] = tqs.map(_.kpath_(c)).map(c.field(_).info.containee)

      // ---------------------------------------------------------------------------
      @deprecated("see 210111095156") def puts0(c: Cls, from:  Containee)                    (implicit ev: $Target <:< KPath): Cls = puts0(c, Seq(from))
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
