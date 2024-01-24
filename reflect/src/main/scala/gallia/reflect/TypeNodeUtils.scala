package gallia
package reflect

// ===========================================================================
object TypeNodeUtils {

  /** should return some if valid, just removing any Option/Seq containers */
  private[reflect] def validContainerOpt(dis: TypeNode): Option[TypeLeaf] =
      _validContainerOpt(dis)
        .flatMap { node => node.args match {
            case Nil => Some(node.leaf)
            case _   => None } }

    // ---------------------------------------------------------------------------
    private def _validContainerOpt(dis: TypeNode): Option[TypeNode] = { import dis._
      // TODO: issue with eg Pes[T] = Option[Seq[T]]
           if (leaf.isSeq) Some(args.head)
      else if (leaf.isOption || leaf.isSome) // TODO: None? parameterized with Nothing..
        if (args.size == 1 && args.head.leaf.isSeq) Some(args.head.args.head)
        else                                        Some(args.head)
      else if (args.size == 0)                      Some(dis)
      else                                          None } }

// ===========================================================================
