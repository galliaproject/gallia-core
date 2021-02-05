package gallia.reflect

import aptus.{Anything_, String_}

// ===========================================================================
object TypeNodeUtils {

  /** should return some if valid, just removing any Option/Seq containers */
  def validContainerOpt(dis: TypeNode): Option[TypeLeaf] =
      _validContainerOpt(dis)
        .flatMap { node => node.args match {
            case Nil => Some(node.leaf)
            case seq => None } }

    // ---------------------------------------------------------------------------
    private def _validContainerOpt(dis: TypeNode): Option[TypeNode] = { import dis._
      // TODO: issue with eg Pes[T] = Option[Seq[T]]
      (      if (leaf.isSeq   ) Some(args.head)
        else if (leaf.isOption || leaf.isSome) // TODO: None? parameterized with Nothing..
          if (args.size == 1 && args.head.leaf.isSeq) Some(args.head.args.head)
          else                                        Some(args.head)
        else if (args.size == 0)                      Some(dis)
        else                                          None)
    }

  // ===========================================================================
  // eg for translate type mismatch
  def formatSuccinct(dis: TypeNode): String = { import dis._

      def tmp(value: TypeLeaf) = value.alias.getOrElse(value.name)

      def isTopLevel(value: TypeNode): Boolean =
        !value.complex &&
        BasicType.isKnown(value.leaf.name)

      // ---------------------------------------------------------------------------
      containerTypeOpt match {
        case Some(container) =>
          val leaf = args.head.leaf.thn(tmp)

          "succinct".colon(container match {
              case Container._Opt => leaf.surroundWith(    "Option[", "]")
              case Container._Nes => leaf.surroundWith(       "Seq[", "]")
              case Container._Pes => leaf.surroundWith("Option[Seq[", "]]")
              case _              => ??? }) // TODO:can't actually happen - create sub-enum

        case None =>
          if (isTopLevel(dis)) s"succinct:${tmp(leaf)}"
          else                 formatDefault
      }
    }
}

// ===========================================================================
