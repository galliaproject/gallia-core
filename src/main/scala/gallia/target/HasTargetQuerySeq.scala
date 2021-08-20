package gallia.target

import aptus.Anything_

import gallia._
import gallia.target.utils.TypedTargetQueryUtils
import gallia.vldt.MetaValidation

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

  def vldtAsOrigin(c: Cls): Errs =
    tqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsOrigin(c)) ++ 
    MetaValidation.distinctRPathz(__qpathz(c))

  def vldtAsNewDestination(c: Cls): Errs =
    tqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsNewDestination(c)) ++ 
    MetaValidation.distinctRPathz(__qpathz(c))
}

// ===========================================================================
