package gallia.heads

import gallia._
import gallia.env._

// ===========================================================================
@aptus.pseudosealed trait Head[F <: Head[_]] { // useful restrict eg f: HeadZ => $Head such that $Head <: Head
    protected[gallia] val nodeId : NodeId
    protected[gallia] val handler: Handler

    protected[gallia] final def end(): HeadEnd = handler.end(this)

    protected[gallia] final def underlyingDagHasOnlyOutputLeaves: Boolean =
      retrieveDag
        .leaves
        .forall { case (_, node) =>
          // by design...; TODO: create common XO trait
          node.isInstanceOf[ActionUO] ||
          node.isInstanceOf[ActionZO] }

    // ---------------------------------------------------------------------------
    private def retrieveDag: ActionDag = handler.env.retrieveDagFromNode(nodeId)
  }

  // ===========================================================================
  object Head {
    def inputV[T: WTT](a: T): HeadV[T] = inputV[T](new gallia.actions.in.InMemoryInputV(a))

    // ===========================================================================
    private[gallia] def inputU        (input: ActionIU): HeadU    = inputU(env.Global)(input)
    private[gallia] def inputZ        (input: ActionIZ): HeadZ    = inputZ(env.Global)(input)
    private[gallia] def inputV[T: WTT](input: ActionIV): HeadV[T] = inputV(env.Global)(input)

      // ---------------------------------------------------------------------------
      // also for nesting
      private[gallia] def inputU        (env: Env)(input: ActionIU): HeadU    = new Handler(env).startu(input)
      private[gallia] def inputZ        (env: Env)(input: ActionIZ): HeadZ    = new Handler(env).startz(input)
      private[gallia] def inputV[T: WTT](env: Env)(input: ActionIV): HeadV[T] = new Handler(env).startv(input)
  }

// ===========================================================================
