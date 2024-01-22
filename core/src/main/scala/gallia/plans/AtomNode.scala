package gallia
package plans

import dag.{RootId, HasNodeId}

// ===========================================================================
case class AtomNode(
      id     : NodeId,
      atom   : Atom,
      debug  : AtomNodeDebugging,
      moreIds: Seq[NodeId] = Nil) // temporary hack (see 210611124807)    
    extends HasNodeId {

  def process(input: DataInput)(missingInputs: RootId => NDT): NDT =
      AtomProcessor(input, missingInputs)(
        id, atom)(
        afferentSchemas, efferentSchema)(
        debug)

    // ---------------------------------------------------------------------------
    private def afferentSchemas: Clss = debug.ctx.afferents.pipe(Clss.apply)
    private def efferentSchema : Cls  = debug.ctx.efferent

  // ===========================================================================
  def formatSuccinct1 = s"${id}:${atom.formatSuccinct1.replace("$", "")}"

  def isNested = atom.isPlaceholder
  def isIdentity = atom.isIdentityUU || atom.isIdentityZZ

def isBar = isNested || isIdentity  
}

// ---------------------------------------------------------------------------
case class AtomNodeDebugging(    
    parentId: NodeId, // ActionNode's
    ctx     : NodeMetaContext,
    origin  : CallSite)
    
// ===========================================================================
