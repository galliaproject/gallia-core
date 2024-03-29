package gallia
package selection
package untyped

import trgt._
import trgt.utils.TargetQueryValidation._
import selection.typed._
import selection.typed.fluency.TsSelBundles
import selection.untyped.processors._

// ===========================================================================
object UtsBase {
  import selection.untyped.{UtsOps => Origins}

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

      def resolve(sel: Selector) = sel(origin).pipe(tqkpath)
    }

    // ---------------------------------------------------------------------------
    object ForPath extends UtsBase {
      type Origin = Origins.ForPath
      type Target = Targets.ForPath
      val origin = new Origin {}

      def resolve(sel: Selector) = sel(origin).pipe(tqkpath)
    }

    // ---------------------------------------------------------------------------
    object ForEachKey extends UtsBase {
      type Origin = Origins.ForEachKey
      type Target = Targets.ForEachKey
      val origin = new Origin {}

      def resolve(sel: Selector) = sel(origin).pipe(tqkpathz)
    }

    // ---------------------------------------------------------------------------
    object ForEachPath extends UtsBase {
      type Origin = Origins.ForEachPath
      type Target = Targets.ForEachPath
      val origin = new Origin {}

      def resolve(sel: Selector) = sel(origin).pipe(tqkpathz)
    }

  // ===========================================================================
  object CommonTyped extends KPathBase { type Origin = Origins.CommonTyped  ; val origin = new Origin {} }

  // ---------------------------------------------------------------------------
  object RemoveIf    extends UtsBase { type Origin = Origins.RemoveIf }

  // ---------------------------------------------------------------------------
  object RemoveValueFor extends KeyBase { type Origin = Origins.RemoveValueFor; val origin = new Origin {} }
  object SetDefaultFor  extends KeyBase { type Origin = Origins.SetDefaultFor ; val origin = new Origin {} }

  // ---------------------------------------------------------------------------
  object Rename extends KPathBase { type Origin = Origins.Rename; val origin = new Origin {} }

  // ---------------------------------------------------------------------------
  object ReorderAsX extends KeyzBase { type Origin = Origins.ReorderAsX; val origin = new Origin {} }

  // ---------------------------------------------------------------------------
  object ModifyEnumValuesFor extends RPathzBase { type Origin = Origins.ModifyEnumValuesFor; val origin = new Origin {} }

  // ---------------------------------------------------------------------------
  object Remove     extends KPathzBase { type Origin = Origins.Remove    ; val origin = new Origin {} }
  object Retain     extends RPathzBase { type Origin = Origins.Retain    ; val origin = new Origin {} }
  object SetDefault extends RPathzBase { type Origin = Origins.SetDefault; val origin = new Origin {} }
  object Convert    extends RPathzBase { type Origin = Origins.Convert   ; val origin = new Origin {} }
  object Translate  extends RPathzBase { type Origin = Origins.Translate ; val origin = new Origin {} }
  object Custom     extends RPathzBase { type Origin = Origins.Custom    ; val origin = new Origin {} }

  object Nest       extends RPathzBase { type Origin = Origins.Nest      ; val origin = new Origin {} }
  object Renesting  extends KeyzBase   { type Origin = Origins.Renesting ; val origin = new Origin {} }
  object UnnestFrom extends RenzBase   { type Origin = Origins.UnnestFrom; val origin = new Origin {} }

  object Transform  extends KPathzBase { type Origin = Origins.Transform ; val origin = new Origin {} }
  object FilterBy   extends KPathzBase { type Origin = Origins.FilterBy  ; val origin = new Origin {} }

  // ===========================================================================
  object Reducing extends KeyzBase { type Origin = Origins.Reducing; val origin = new Origin {} }

  // ---------------------------------------------------------------------------
  object SortingSingle   extends KPathBase  { type Origin = Origins.SortingSingle  ; val origin = new Origin {} }
  object SortingMultiple extends KPathzBase { type Origin = Origins.SortingMultiple; val origin = new Origin {} }

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
        trait HasSelBasic extends
                 HasOneSelBasic
            with HasOptSelBasic
            with HasNesSelBasic
            with HasPesSelBasic
            with HasXSelBasic
            with HasTypedSelBasic

          // ---------------------------------------------------------------------------
          trait HasOneSelBasic   extends TsSelBundles.HasSelOneBasic  [Origin, Target]
          trait HasOptSelBasic   extends TsSelBundles.HasSelOptBasic  [Origin, Target]
          trait HasNesSelBasic   extends TsSelBundles.HasSelNesBasic  [Origin, Target]
          trait HasPesSelBasic   extends TsSelBundles.HasSelPesBasic  [Origin, Target]
          trait HasXSelBasic     extends TsSelBundles.HasSelXBasic    [Origin, Target]
          trait HasTypedSelBasic extends TsSelBundles.HasSelTypedBasic[Origin, Target]
    }

    // ===========================================================================
    trait RPathzBase extends UtsBase { val origin: Origin
      type Target = RPathzSelection
      def resolve(sel: Selector): TqRPathz = sel(origin).pipe(tqrpathz) }

    // ---------------------------------------------------------------------------
    trait KPathzBase extends UtsBase { val origin: Origin
      type Target = KPathzSelection
      def resolve(sel: Selector): TqKPathz = sel(origin).pipe(tqkpathz) }

    // ---------------------------------------------------------------------------
    trait KPathBase extends UtsBase { val origin: Origin
      type Target = KPathSelection
      def resolve(sel: Selector): TqKPath = sel(origin).pipe(tqkpath) }

    // ---------------------------------------------------------------------------
    trait KeyzBase extends UtsBase { val origin: Origin
      type Target = KeyzSelection
      def resolve(sel: Selector): TqKeyz = sel(origin).pipe(tqkeyz) }

    // ---------------------------------------------------------------------------
    trait RenzBase extends UtsBase { val origin: Origin
      type Target = RenzSelection
      def resolve(sel: Selector): TqRenz = sel(origin).pipe(tqrenz) }

    // ---------------------------------------------------------------------------
    trait RenBase extends UtsBase { val origin: Origin
      type Target = RenSelection
      def resolve(sel: Selector): TqRen = sel(origin).pipe(tqren) }

    // ---------------------------------------------------------------------------
    trait KeyBase extends UtsBase { val origin: Origin
      type Target = KeySelection
      def resolve(sel: Selector): TqKey = sel(origin).pipe(tqkey) }
    
}

// ===========================================================================
