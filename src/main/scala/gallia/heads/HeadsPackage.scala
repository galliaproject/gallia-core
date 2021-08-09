package gallia

// ===========================================================================
package object heads extends _heads

  // ---------------------------------------------------------------------------
  package heads {
    trait _heads { // TODO: generalize
      type WTT[T] = scala.reflect.runtime.universe.WeakTypeTag[T]

      type NodeId  = gallia.dag.NodeId
      type Handler = gallia.env.Handler

      type WV = gallia.Whatever
      type W  = gallia.Whatever

      type WV1    = gallia.Whatever
      type WV2[T] = gallia.whatever.TypedWhatever[T]

      val _Error = gallia.vldt._Error

      // ---------------------------------------------------------------------------
      private[heads] type TypedTargetQuery[T] = gallia.target.TypedTargetQuery[T]
      private[heads] val  TypedTargetQuery    = gallia.target.TypedTargetQuery

      // ---------------------------------------------------------------------------
      private[gallia] val TSL = gallia.selection.typed  . TsBoilerplate
      private[gallia] val SEL = gallia.selection.untyped.UtsBoilerplate

      // ---------------------------------------------------------------------------
      type TqRPathz = gallia.selection.typed.TqRPathz
      type TqKeyz   = gallia.selection.typed.TqKeyz
      type TqKPath  = gallia.selection.typed.TqKPath
    }
  }

// ===========================================================================