package gallia.plans

import gallia.dag.HasNodeId
import gallia.dag.NodeId
import gallia._

// ===========================================================================
case class AtomNode(
      id     : NodeId,
      atom   : Atom,
      debug  : AtomNodeDebugging,
      moreIds: Seq[NodeId] = Nil) // temporary hack (see 210611124807)    
    extends HasNodeId {
  
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