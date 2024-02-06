package gallia
package oswo
package source

import aptus._

// not meant to be very generic

// ===========================================================================
case class _Name(value: String) extends SourceCode { // TODO: validations
  require(value.nonEmpty) // TODO: more

  override def toString: String = format
    def format: SourceString = value }

// ===========================================================================
case class _Object(
      name    : _Name,
      extendss: Seq[String],
      body    : SourceCode)
    extends SourceCode {

  final override def format: SourceString =
    s"""|object ${name.format} ${formatExtends} {
        |
        |  ${body.format}
        |
        |}""".stripMargin

  // ---------------------------------------------------------------------------
  private def formatExtends: SourceString =
    extendss match {
      case Nil           => ""
      case Seq(sole)     => s"extends ${sole}"
      case first +: more => s"extends ${first} ${more.join("with ")}" /* TODO: indent */ } }

// ===========================================================================
case class _MethodDefinition(
      name      : _Name,
      params    : Seq[Any],
      returnType: String,
      body      : String)
    extends SourceCode {

  final override def format: SourceString =
    s"""|def ${name.format}(${params.join(", " /* TODO*/)}): ${returnType} = {
        |${body.indentAll(2)}
        |}""".stripMargin }

// ===========================================================================
case class _MethodCall(
      name:     _Name,
      args: Seq[String])
    extends SourceCode {

  final override def format: SourceString =
    s"""|${name.format}(
        |${args.joinln.indentAll}
        |)""".stripMargin }

// ===========================================================================
case class _ArgAssignment(lhs: _Name, rhs: _Name) extends SourceCode {
  final override def format: SourceString =
    s"${lhs.format} = ${rhs.format}" /* TODO: padding */ }

// ===========================================================================
