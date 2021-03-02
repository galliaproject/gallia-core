package gallia.heads

import gallia._
import gallia.env._
import gallia.dag._

// ===========================================================================
object HeadsNestingHandler { // TODO: move thse to handler/handler helper

  /** typically for nesting, whereby input schema will be provided when parent gets processed */
  trait NestingMetaPlaceholder extends ActionVM0 {
      // can't happen by design (replaced by actual input first, see ?)
      def  vldt: Errs = illegal("bug:201110144401")
      def _meta: Cls  = illegal("bug:201110144402") }

    // ---------------------------------------------------------------------------
    object NestingMetaPlaceholderU extends NestingMetaPlaceholder with ActionIUa { def atomius = Nil }
    object NestingMetaPlaceholderZ extends NestingMetaPlaceholder with ActionIZa { def atomizs = Nil }
    object NestingMetaPlaceholderV extends NestingMetaPlaceholder with ActionIVa { def atomivs = Nil }

  // ===========================================================================
  // FIXME: disable forking in these case

  private[gallia] def uToV[T](f: HeadU => HeadV[T]): (RootId, ActionDag, LeafId) = {
    val head   = Head.inputU(NestingMetaPlaceholderU)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def zToV[T](f: HeadZ => HeadV[T]): (RootId, ActionDag, LeafId) = {
    val head   = Head.inputZ(NestingMetaPlaceholderZ)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def uToU(f: HeadU => HeadU): (RootId, ActionDag, LeafId) = {
    val head   = Head.inputU(NestingMetaPlaceholderU)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def zToZ(f: HeadZ => HeadZ): (RootId, ActionDag, LeafId) = {
    val head   = Head.inputZ(NestingMetaPlaceholderZ)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def uToZ(f: HeadU => HeadZ): (RootId, ActionDag, LeafId) = {
    val head   = Head.inputU(NestingMetaPlaceholderU)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def zToU(f: HeadZ => HeadU): (RootId, ActionDag, LeafId) = {
    val head   = Head.inputZ(NestingMetaPlaceholderZ)
    val leafId = f(head).nodeId // populates DAG

    (head.nodeId, Env.retrieveDagFromNode(leafId), leafId)
  }

  // ---------------------------------------------------------------------------
  private[gallia] def uuToU(f: (HeadU, HeadU) => HeadU): ((RootId, RootId), ActionDag, LeafId) = {
    val head1 = Head.inputU(NestingMetaPlaceholderU)
    val head2 = Head.inputU(NestingMetaPlaceholderU)

    val leafId = f(head1, head2).nodeId // populates DAG

    ((head1.nodeId, head2.nodeId), Env.retrieveDagFromNode(leafId), leafId)
  }

}

// ===========================================================================
