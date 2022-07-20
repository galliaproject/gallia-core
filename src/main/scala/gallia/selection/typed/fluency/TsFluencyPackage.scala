package gallia
package selection
package typed

// ===========================================================================
package object fluency {
  private[typed] type One_[T] = TsWrapper[           T  ]
  private[typed] type Opt_[T] = TsWrapper[Option    [T] ]
  private[typed] type Nes_[T] = TsWrapper[Seq       [T] ]
  private[typed] type Pes_[T] = TsWrapper[Option[Seq[T]]]

  // ---------------------------------------------------------------------------
  object TsBundles {
    trait HasRequiredBasic[$Wrap, $Wrapz <: Seq[$Wrap]] extends
             TsSingleBundles  .HasSingleRequiredBasic  [$Wrap]
        with TsRepeatedBundles.HasRepeatedRequiredBasic[$Wrap, $Wrapz]
  }
}

// ===========================================================================
