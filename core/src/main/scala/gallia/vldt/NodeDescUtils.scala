package gallia
package vldt

import aptus.{Anything_, Seq_, String_}
import aptus.ErrorMsg

import reflect._

// ===========================================================================
object NodeDescUtils {
  def isValid      (value: NodeDesc): Boolean     = !value.isInvalid
  def errorMessages(value: NodeDesc): Seq[String] =  value.errorMessages

  // ===========================================================================
  private implicit class ClsDesc_(u: ClsDesc) { import u._

    def isInvalid: Boolean =
     /**/ name.invalid   ||
     /**/ generic        ||
     /**/ fields.isEmpty ||
     /**/!fields.map(_.key).isDistinct ||
     /**/ fields.exists(_.isInvalid)

    // ---------------------------------------------------------------------------
    def errorMessages                : Seq[ErrorMsg] = errorMessages(Parent.Root)
    def errorMessages(parent: Parent): Seq[ErrorMsg] =
      NodeDescUtils.errorMessages(
         name.invalid                 -> s"${ErrorId.InvalidClassName}: ${name.value}",
         generic                      -> s"${ErrorId.GenericClass}",
         fields.isEmpty               -> s"${ErrorId.NoFields}",
        !fields.map(_.key).isDistinct -> s"${ErrorId.DuplicateKeys}: ${fields.map(_.key).duplicates.distinct.#@@}") ++
      fields.flatMap(_.errorMessages(parent.append(name.value)))

  }

  // ===========================================================================
  private implicit class FldDesc_(u: FldDesc) { import u._

    def isInvalid: Boolean =
      key.invalid ||
      tipe.isInvalid

    // ---------------------------------------------------------------------------
    def errorMessages(parent: Parent): Seq[ErrorMsg] =
      NodeDescUtils.errorMessages(key.invalid -> s"${ErrorId.Runtime.InvalidKey} - ${parent} - invalid key ${key.value}") ++
      tipe.errorMessages(parent)

  }

  // ===========================================================================
  private implicit class NodeDesc_(u: NodeDesc) { import NodeDesc._

    def isInvalid: Boolean =
      u match {
        case Error(_)         => true
        case Enumeratum       => false
        case Named(fullName)  => !fullName.isGalliaEnumValue && !BasicType.isKnown(fullName)
        case Nesting(nesting) => nesting.isInvalid }

    // ===========================================================================
    def errorMessages                : Seq[ErrorMsg] = errorMessages(Parent.Root)
    def errorMessages(parent: Parent): Seq[ErrorMsg] =
      u match {
        case Error(node) => s"${ErrorId.InvalidTypeNode} - ${parent} ${node.formatDefault}".in.seq
        case Enumeratum  => Nil
        case Named(fullName) =>
          if (BasicType.isKnown(fullName) || fullName.format == BasicType.ScalaAnyFullName) Nil
          else s"${ErrorId.UnsupportedTlSubtype} - ${parent} - ${fullName.format.quote}".in.seq
        case Nesting(nesting) => nesting.errorMessages(parent) } }

  // ===========================================================================
  private def errorMessages(pairs: (Boolean, ErrorMsg)*): Seq[ErrorMsg] =
    pairs
      .toList
      .flatMap { case (test, value) =>
        if (!test) Nil
        else       Seq(value) }

}

// ===========================================================================
