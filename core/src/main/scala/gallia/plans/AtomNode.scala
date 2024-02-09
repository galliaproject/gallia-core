package gallia
package plans

import dag._

// ===========================================================================
case class AtomNode(
      id     : NodeId,
      ctx    : AtomMetaContext,
      atom   : Atom,
      moreIds: Seq[NodeId] = Nil /* temporary hack (see 210611124807) */)
    extends HasNodeId
    with    HasNodeContext[AtomMetaContext]
    with    HasNodeTarget [Atom] {
  protected val ctxOpt = Some(ctx)
  protected val target = atom

  // ---------------------------------------------------------------------------
  def process(input: DataInput)(missingInputs: RootId => NDT): NDT =
    AtomProcessor(ctx)(input, missingInputs)(id, atom)

  // ===========================================================================
  def formatSuccinct1 = s"${id}:${atom.formatSuccinct1.replace("$", "")}"

  def isNested   = atom.isPlaceholder
  def isIdentity = atom.isIdentityUU || atom.isIdentityZZ

  def isNestedOrIdentity = isNested || isIdentity }

// ===========================================================================
