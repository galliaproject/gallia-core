package gallia
package heads

// ===========================================================================
@aptus.pseudosealed trait Head[F <: Head[_]] { // useful restrict eg f: HeadZ => $Head such that $Head <: Head
    protected[gallia] val nodeId : NodeId
    protected[gallia] val handler: Handler

    protected[gallia] final def end(): HeadEnd = handler.end(this)

    protected[gallia] final def underlyingDagHasOnlyOutputLeaves: Boolean =
      retrieveDag.leaves.forall(_.isActionXO /* by design */) // TODO: create common XO trait

    // ---------------------------------------------------------------------------
    private def retrieveDag: plans.ActionMetaDag = env.Env.retrieveDagFromNode(nodeId) }

  // ===========================================================================
  object Head {
    def inputV[T: WTT](a: T): HeadV[T] = inputV[T](gallia.actions.in.InMemoryInputV(a))

    // ===========================================================================
    // also for nesting
    private[gallia] def inputU        (input: ActionIU): HeadU    = new Handler().startu   (input)
    private[gallia] def inputZ        (input: ActionIZ): HeadZ    = new Handler().startz   (input)
    private[gallia] def inputV[T: WTT](input: ActionIV): HeadV[T] = new Handler().startv[T](input) }

// ===========================================================================
