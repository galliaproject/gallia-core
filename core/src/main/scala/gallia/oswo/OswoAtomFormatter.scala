package gallia
package oswo

import aptus._

// ===========================================================================
object OswoAtomFormatter {
  import OswoAtomFormatterUtils._
  import OswoAtomFieldFormatter._
  import OswoAtomCommon._
  import source.SourceFluentBuilders._

  // ---------------------------------------------------------------------------
  def inMemoryInputUb(atom: _InMemoryInputUb)(efferent: Cls)(ctx: OswoCtx): SourceString = { import ctx._
    val atomName = className(atom)
    val ccs = efferent.formatSourceLines(id)(atomName)

    // ---------------------------------------------------------------------------
    val aobj = atom.value.forceAObj

    val methodName    = id.method(atomName) // eg m_1_1_TransformVV
    val dataClassName = id.cc    (atomName) // eg dc_1_1

    val method =
      ().methodDefinition(methodName)
        .returnType(dataClassName)
        .body {
          ().applyCallOn(dataClassName).args {
            efferent
              .fields
              .map(_.fieldInitialization(aobj.o))
              .map(_.format.surroundWith("    ", ",")) } }

    // ---------------------------------------------------------------------------
    format(ccs, method.format, _call0(id)(last)(atomName)) }

  // ===========================================================================
  def jsonObjectString(atom: _JsonObjectString)(efferent: Cls)(ctx: OswoCtx): SourceString = { import ctx._
    val name = className(atom)
    val ccs  = efferent.formatSourceLines(id)(name)

    val method = ??? // TODO

    format(ccs, method, _call0(id)(last)(name)) }

  // ===========================================================================
  def rename(atom: _Rename)(ctx: OswoCtx): SourceString =
    atom.formatCommon2(ctx)(name = common2(atom)(ctx.id)(_.size)) {
      atom._metaIO.in.fields.map(_.fieldRename(atom.value)) }

  // ===========================================================================
  def add(atom: _Add)(ctx: OswoCtx): SourceString =
    atom.formatCommon2(ctx)(name = commonAdd(atom)(ctx.id)) {
      atom._metaIO.out.fields.map(_.fieldAdd(target = atom.key, atom.value)) }

  // ---------------------------------------------------------------------------
  // replace = add

  // ===========================================================================
  def remove(atom: _Remove)(ctx: OswoCtx): SourceString = {
//atom: not really needed here beyond classes
    atom.formatCommon2(ctx)(name = commonRemove(atom)(ctx.id)) {
      atom._metaIO.out.fields.map(_.fieldDefault) } }

  // ===========================================================================
  def transformVV(atom: _TransformVV)(ctx: OswoCtx): SourceString = {
    atom.pair.path.tailPair match {
        case (parent, Some(tail)) => ???
        case (leaf: Key, None)    =>
          atom.formatCommon(ctx) {
            atom._metaIO.out.fields.map(_.fieldFunctionIf(ctx.id, leaf)) } } }

  // ===========================================================================
  def convertToDouble(atom: _ConvertToDouble)(ctx: OswoCtx): SourceString = {
    atom.target.path.tailPair match {
      case (parent, Some(tail)) => ???
      case (leaf: Key, None) =>
        atom.formatCommon(ctx) {
          atom._metaIO.in.fields.map(_.fieldConvertToDouble(leaf)) } }}

}

// ===========================================================================
