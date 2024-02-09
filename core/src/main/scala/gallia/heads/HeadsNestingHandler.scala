package gallia
package heads

import plans.ActionMetaDag
import env._
import dag._

// ===========================================================================
object HeadsNestingHandler { // TODO: move thse to handler/handler helper

  /** typically for nesting, whereby input schema will be provided when parent gets processed */
  trait NestingMetaPlaceholder extends ActionVM0 {
      // can't happen by design (replaced by actual input first, see ?)
      def  vldt: Errs = aptus.illegalState("bug:201110144401")
      def _meta: Cls  = aptus.illegalState("bug:201110144402") }

    // ---------------------------------------------------------------------------
    object NestingMetaPlaceholderU extends NestingMetaPlaceholder with actions.boilerplate.ActionBoilerplate.ActionIU0N { def atomius = Nil }
    object NestingMetaPlaceholderZ extends NestingMetaPlaceholder with actions.boilerplate.ActionBoilerplate.ActionIZ0N { def atomizs = Nil }
    object NestingMetaPlaceholderV extends NestingMetaPlaceholder with actions.boilerplate.ActionBoilerplate.ActionIV0N { def atomivs = Nil }

  // ===========================================================================
  // FIXME: disable forking in these case

  private[gallia] def uToV[T](f: HeadU => HeadV[T]): (RootId, ActionMetaDag, LeafId) = {
    val head   = Head.inputU(NestingMetaPlaceholderU)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def zToV[T](f: HeadZ => HeadV[T]): (RootId, ActionMetaDag, LeafId) = {
    val head   = Head.inputZ(NestingMetaPlaceholderZ)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def uToU(f: HeadU => HeadU): (RootId, ActionMetaDag, LeafId) = {
    val head   = Head.inputU(NestingMetaPlaceholderU)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def zToZ(f: HeadZ => HeadZ): (RootId, ActionMetaDag, LeafId) = {
    val head   = Head.inputZ(NestingMetaPlaceholderZ)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def uToZ(f: HeadU => HeadZ): (RootId, ActionMetaDag, LeafId) = {
    val head   = Head.inputU(NestingMetaPlaceholderU)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def zToU(f: HeadZ => HeadU): (RootId, ActionMetaDag, LeafId) = {
    val head   = Head.inputZ(NestingMetaPlaceholderZ)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def uuToU(f: (HeadU, HeadU) => HeadU): ((RootId, RootId), ActionMetaDag, LeafId) = {
    val head1 = Head.inputU(NestingMetaPlaceholderU)
    val head2 = Head.inputU(NestingMetaPlaceholderU)

    val leafId = f(head1, head2).nodeId // populates DAG

    ((head1.nodeId, head2.nodeId), Env.retrieveDagFromNode(leafId), leafId)
  }

}

// ===========================================================================
