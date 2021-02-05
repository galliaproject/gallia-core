package gallia.dag

import aptus.{Anything_, Seq_}
import aptus.Nes

// ===========================================================================
case class Chain[$NodeType](
    nodes: Nes[$NodeType],
    idResolver: $NodeType => NodeId) {

  nodes
    .map(idResolver)
    .require(_.nonEmpty)
    .requireDistinct()

  override def toString: String = formatDefault

    def formatDefault: String = nodes.mkString("[", " -> ", "]")

  // ---------------------------------------------------------------------------
  def nodeIds: Seq[NodeId] = nodes.map(idResolver)

  def edges: Seq[Edge] = nodeIds match {
    case Nil | Seq(_) => Nil
    case more         => more.sliding(2).map(_.force.tuple2).toSeq }

  def headId: NodeId = idResolver(head)
  def lastId: NodeId = idResolver(last)

  // ---------------------------------------------------------------------------
  def size = nodes.size

  def head = nodes.head
  def last = nodes.last

  // ---------------------------------------------------------------------------
  def pair =
         if (size == 1) (head, None)
    else if (size == 2) (head, Some(Nil            , last))
    else                (head, Some(nodes.tail.init, last))
}

// ===========================================================================
object Chain {
  def idChain(nodes: Nes[NodeId]) = Chain[NodeId](nodes, identity)
}

// ===========================================================================