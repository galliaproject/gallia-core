package gallia
package reflect

import aptus.Seq_

// ===========================================================================
case class ClsDesc( // TODO: rename
      name         : NameDesc, // TODO: alias too
      generic      : Boolean,  // TODO: allow in the future?
      fields       : Seq[FldDesc] ) {

    def empty = fields.isEmpty

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      def formatDefault: String =
        s"${name}\t${generic}\t${fields.map(_.formatDefault).section2}" }

  // ===========================================================================
  object ClsDesc {

    def from(leaf: TypeLeaf, generic: Boolean): ClsDesc =
      ClsDesc(
        NameDesc(
            leaf.fullName.format,
            !KeyValidation.isValid(leaf.fullName.format)),
        generic,
        leaf.fields.map(FldDesc.from)) }

// ===========================================================================
case class FldDesc(key: NameDesc, tipe : NodeDesc) {
    override def toString: String = formatDefault
      def formatDefault: String = s"${key}\t${tipe}" }

  // ---------------------------------------------------------------------------
  object FldDesc {
    def from(field: Field) = {
      FldDesc(
          NameDesc(
              field.key,
              !KeyValidation.isValid(field.key)),
          NodeDesc.from(field.typeNode) ) } }

// ===========================================================================
case class NameDesc(value: String, invalid: Boolean) {
  override def toString: String = formatDefault
    def formatDefault: String = s"|${value}|${if (invalid) " (invalid)" else " (valid)"}" }

// ===========================================================================
sealed trait NodeDesc

  // ---------------------------------------------------------------------------
  object NodeDesc {

    case class  Error   (node: TypeNode)           extends NodeDesc
    case class  Nesting (nested: ClsDesc)          extends NodeDesc
    case class  Named   (name: FullyQualifiedName) extends NodeDesc
    case object Enumeratum                         extends NodeDesc

    // ---------------------------------------------------------------------------
    def from(node: TypeNode): NodeDesc =
      node.validContainerOpt match {
        case None                                        => Error(node)
        case Some(fieldType) if (fieldType.dataClass)    => Nesting(ClsDesc.from(fieldType, generic = false))
        case Some(fieldType) if (fieldType.isEnumeratum) => Enumeratum // not actually used at the moment
        case Some(fieldType)                             => Named(fieldType.fullName) } }

// ===========================================================================
