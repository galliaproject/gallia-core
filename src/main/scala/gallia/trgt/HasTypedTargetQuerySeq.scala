package gallia.target

//import aptus.Seq_
//
import gallia._
import gallia.vldt.MetaValidation
import gallia.vldt.SpecialCardiMode

// ===========================================================================
trait HasTypedTargetQuerySeq[$Target] extends HasTargetQuerySeq[$Target] {
  def ttqs: Seq[TypedTargetQuery[$Target]]

  // ---------------------------------------------------------------------------
  final override def tqs: Seq[TargetQuery[$Target]] = ttqs.map(_.tq)

  // ===========================================================================
  // vldt

  override
  def vldtAsOrigin        (c: Cls)                        : Errs = vldtAsOrigin(c, mode = SpecialCardiMode.Normal)
  def vldtAsOrigin        (c: Cls, mode: SpecialCardiMode): Errs = ttqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsOrigin(c))         ++ MetaValidation.distinctRPathz(__qpathz(c))
  
  override
  def vldtAsNewDestination(c: Cls)                        : Errs = ttqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsNewDestination(c)) ++ MetaValidation.distinctRPathz(__qpathz(c))
  def vldtAsAnyDestination(c: Cls)                        : Errs = ttqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsAnyDestination(c)) ++ MetaValidation.distinctRPathz(__qpathz(c))

  // ---------------------------------------------------------------------------
  def vldtAsCotransformDestination(c: Cls, from: KPath) (implicit ev: $Target <:< gallia.KPath): Errs = vldtAsCotransformDestination(c, KPathz(Seq(from)))        
  def vldtAsCotransformDestination(c: Cls, from: KPathz)(implicit ev: $Target <:< gallia.KPath): Errs =
    ttqs.foldLeft(Seq[Err]())(_ ++ _.vldtAsCotransformDestination(c, from)) ++ MetaValidation.distinctRPathz(__qpathz(c))
}

// ===========================================================================
