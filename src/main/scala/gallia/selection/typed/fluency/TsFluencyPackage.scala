package gallia
package selection.typed

// ===========================================================================
package object fluency {
  private[typed] type One_[T] = TsWrapper[           T  ]
  private[typed] type Opt_[T] = TsWrapper[Option    [T] ]
  private[typed] type Nes_[T] = TsWrapper[Seq       [T] ]
  private[typed] type Pes_[T] = TsWrapper[Option[Seq[T]]]

  // ===========================================================================
  object TsNestingIndividuals {
    //TODO: rename, these also abstract requiredness
    trait HasOneObj [$Wrap] { def obj  (target: $Wrap) = new One_[gallia.HeadU](target) }
    trait HasOneObjz[$Wrap] { def objz (target: $Wrap) = new One_[gallia.HeadZ](target) }
  }
}

// ===========================================================================