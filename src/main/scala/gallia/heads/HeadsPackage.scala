package gallia

// ===========================================================================
package object heads extends _heads

  // ---------------------------------------------------------------------------
  package heads {
    trait _heads { // TODO: generalize
      type NodeId  = dag.NodeId
      type Handler = env.Handler

      type  WV    = whatever.Whatever
      type  W     = whatever.Whatever
      type TWV[T] = whatever.TypedWhatever[T]

      val _Error = vldt._Error

      // ---------------------------------------------------------------------------
      private[gallia] val TSL = selection.typed  . TsBoilerplate
      private[gallia] val SEL = selection.untyped.UtsBoilerplate

      // ---------------------------------------------------------------------------
      type TqRPathz = selection.typed.TqRPathz
      type TqKeyz   = selection.typed.TqKeyz
      type TqKPath  = selection.typed.TqKPath } }

// ===========================================================================
