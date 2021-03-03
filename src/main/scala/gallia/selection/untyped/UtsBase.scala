package gallia.selection.untyped

import aptus.Anything_

import gallia.target._
import gallia.target.utils.TargetQueryValidation._
import gallia.selection.typed._
import gallia.selection.typed.fluency.TsQueryIndividuals
import gallia.selection.untyped.processors._

// ===========================================================================
object UtsBase {
  import gallia.selection.untyped.{UtsOps => Origins}

  // ---------------------------------------------------------------------------
  object Targets {
    type ForKey      = KeySelection
    type ForPath     = KPathSelection
    type ForEachKey  = KeyzSelection
    type ForEachPath = KPathzSelection
  }

  // ===========================================================================
  object ForKey extends UtsBase {
      type Origin = Origins.ForKey
      type Target = Targets.ForKey
      val origin = new Origin {}

      def resolve(sel: Selector) = sel(origin).thn(tqkpath)
    }

    // ---------------------------------------------------------------------------
    object ForPath extends UtsBase {
      type Origin = Origins.ForPath
      type Target = Targets.ForPath
      val origin = new Origin {}

      def resolve(sel: Selector) = sel(origin).thn(tqkpath)
    }

    // ---------------------------------------------------------------------------
    object ForEachKey extends UtsBase {
      type Origin = Origins.ForEachKey
      type Target = Targets.ForEachKey
      val origin = new Origin {}

      def resolve(sel: Selector) = sel(origin).thn(tqkpathz)
    }

    // ---------------------------------------------------------------------------
    object ForEachPath extends UtsBase {
      type Origin = Origins.ForEachPath
      type Target = Targets.ForEachPath
      val origin = new Origin {}

      def resolve(sel: Selector) = sel(origin).thn(tqkpathz)
    }

  // ===========================================================================
  object CommonTyped extends KPathBase { type Origin = Origins.CommonTyped  ; val origin = new Origin {} }

  // ---------------------------------------------------------------------------
  object RemoveIf extends UtsBase { type Origin = Origins.RemoveIf }

  // ---------------------------------------------------------------------------
  object Rename extends KPathBase { type Origin = Origins.Rename; val origin = new Origin {} }

  // ---------------------------------------------------------------------------
  object Remove     extends QpathzBase { type Origin = Origins.Remove    ; val origin = new Origin {} }
  object Retain     extends QpathzBase { type Origin = Origins.Retain    ; val origin = new Origin {} }
  object SetDefault extends QpathzBase { type Origin = Origins.SetDefault; val origin = new Origin {} }
  object Convert    extends QpathzBase { type Origin = Origins.Convert   ; val origin = new Origin {} }
  object Translate  extends QpathzBase { type Origin = Origins.Translate ; val origin = new Origin {} }
  object Custom     extends QpathzBase { type Origin = Origins.Custom    ; val origin = new Origin {} }

  object Renesting  extends KeyzBase   { type Origin = Origins.Renesting ; val origin = new Origin {} }
  object UnnestFrom extends RenzBase   { type Origin = Origins.UnnestFrom; val origin = new Origin {} }

  object Transform  extends KpathzBase { type Origin = Origins.Transform ; val origin = new Origin {} }
  object FilterBy   extends KpathzBase { type Origin = Origins.FilterBy  ; val origin = new Origin {} }

  // ===========================================================================
  object Reducing extends KeyzBase { type Origin = Origins.Reducing; val origin = new Origin {} }

  // ---------------------------------------------------------------------------
  object SortingSingle   extends KPathBase  { type Origin = Origins.SortingSingle  ; val origin = new Origin {} }
  object SortingMultiple extends KpathzBase { type Origin = Origins.SortingMultiple; val origin = new Origin {} }

  // ---------------------------------------------------------------------------
  object Groupee1        extends RenBase  { type Origin = Origins.Groupee1; val origin = new Origin {} }
  object GroupeeN        extends RenzBase { type Origin = Origins.GroupeeN; val origin = new Origin {} }
  object Groupers        extends RenzBase { type Origin = Origins.Groupers; val origin = new Origin {} }

  // ---------------------------------------------------------------------------
  object Merging  extends KeyzBase { type Origin = Origins.Merging ; val origin = new Origin {} }

  // ===========================================================================
  protected trait UtsBase {
        type Origin // TODO: <: Instantiableish?
        type Target
        type Selector = Origin => Target

        // ===========================================================================
        trait HasSels
            extends HasOneSels

          // ---------------------------------------------------------------------------
          @gallia.TypeMatching
          trait HasOneSels
            //TODO: bundles
            extends TsQueryIndividuals.HasOneStringSel [Origin, Target]
            with    TsQueryIndividuals.HasOneIntSel    [Origin, Target]
            with    TsQueryIndividuals.HasOneDoubleSel [Origin, Target]
            with    TsQueryIndividuals.HasOneBooleanSel[Origin, Target]

          // ---------------------------------------------------------------------------
          trait HasOptSels extends
            TsQueryIndividuals.HasOptStringSel [Origin, Target]
            with    TsQueryIndividuals.HasOptIntSel    [Origin, Target]
            with    TsQueryIndividuals.HasOptDoubleSel [Origin, Target]
            with    TsQueryIndividuals.HasOptBooleanSel[Origin, Target]
    }

    // ===========================================================================
    trait QpathzBase extends UtsBase { val origin: Origin
      type Target = RPathzSelection
      def resolve(sel: Selector): TqRPathz = sel(origin).thn(tqqpathz) }

    // ---------------------------------------------------------------------------
    trait KpathzBase extends UtsBase { val origin: Origin
      type Target = KPathzSelection
      def resolve(sel: Selector): TqKPathz = sel(origin).thn(tqkpathz) }

    // ---------------------------------------------------------------------------
    trait KPathBase extends UtsBase { val origin: Origin
      type Target = KPathSelection
      def resolve(sel: Selector): TqKPath = sel(origin).thn(tqkpath) }

    // ---------------------------------------------------------------------------
    trait KeyzBase extends UtsBase { val origin: Origin
      type Target = KeyzSelection
      def resolve(sel: Selector): TqKeyz = sel(origin).thn(tqkeyz) }

    // ---------------------------------------------------------------------------
    trait RenzBase extends UtsBase { val origin: Origin
      type Target = RenzSelection
      def resolve(sel: Selector): TQRenz = sel(origin).thn(tqrenz) }

    // ---------------------------------------------------------------------------
    trait RenBase extends UtsBase { val origin: Origin
      type Target = RenSelection
      def resolve(sel: Selector): TQRen = sel(origin).thn(tqren) }

    // ---------------------------------------------------------------------------
    trait KeyBase extends UtsBase { val origin: Origin
      type Target = KeySelection
      def resolve(sel: Selector): TQKey = sel(origin).thn(tqkey) }
    
}

// ===========================================================================
