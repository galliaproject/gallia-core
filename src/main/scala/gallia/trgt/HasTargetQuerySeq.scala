package gallia
package trgt

import utils.TypedTargetQueryUtils
import vldt.MetaValidation

// ===========================================================================
trait HasTargetQuerySeq[$Target] {
  def tqs: Seq[TargetQuery[$Target]]

  final def  tqkpaths                   (implicit ev: $Target <:< KPath): Seq[TqKPath]  = tqs                    .map(_.asInstanceOf[TqKPath])
  final def ttqkpaths(value: HasTypeSeq)(implicit ev: $Target <:< KPath): Seq[TtqKPath] = tqkpaths.zip(value.hts).map(x => TypedTargetQueryUtils.ttqkpath1(x._1, x._2))

  // ---------------------------------------------------------------------------
  def resolve(c: Cls): Seq[$Target] = tqs.map(_.resolve(c))
  
  // TODO: should be private
  /*private[trgt] */def __kpathz(c: Cls): KPathz = tqs.flatMap(_.__kpaths(c)).pipe(KPathz.apply)
  /*private[trgt] */def __rpathz(c: Cls): RPathz = tqs.flatMap(_.__rpaths(c)).pipe(RPathz.apply)
  
  // ---------------------------------------------------------------------------
  def pathz(c: Cls)(implicit ev: $Target <:< KPath): KPathz = KPathz(resolve(c).asInstanceOf[Seq[KPath]])

  // ===========================================================================
  // vldt

  def vldtAsOrigin(c: Cls): Errs =
    tqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsOrigin(c)) ++ 
    MetaValidation.distinctRPathz(__rpathz(c))

  def vldtAsNewDestination(c: Cls): Errs =
    tqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsNewDestination(c)) ++ 
    MetaValidation.distinctRPathz(__rpathz(c))
}

// ===========================================================================
