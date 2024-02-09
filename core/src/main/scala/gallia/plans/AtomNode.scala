package gallia
package plans

import dag.{RootId, HasNodeId}

// ===========================================================================
case class AtomNode(
        id     : NodeId,
        atom   : Atom,
        ctx    : AtomMetaContext,
        moreIds: Seq[NodeId] = Nil /* temporary hack (see 210611124807) */)
      extends HasNodeId {

    def process(input: DataInput)(missingInputs: RootId => NDT): NDT =
      AtomProcessor(ctx)(input, missingInputs)(id, atom)

    // ===========================================================================
    def formatSuccinct1 = s"${id}:${atom.formatSuccinct1.replace("$", "")}"

    def isNested   = atom.isPlaceholder
    def isIdentity = atom.isIdentityUU || atom.isIdentityZZ

    def isNestedOrIdentity = isNested || isIdentity }

  // ---------------------------------------------------------------------------
  object AtomNode {
    def atomId(id: NodeId, index: Int): NodeId = id + s"-${index}" /* TODO: not if only one? */ }

// ===========================================================================
case class AtomMetaContext(
    actionId     : NodeId,
    actionMetaCtx: ActionMetaContext)
    
// ===========================================================================
