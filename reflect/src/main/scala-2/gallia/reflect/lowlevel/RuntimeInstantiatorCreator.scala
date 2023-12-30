package gallia
package reflect
package lowlevel

import aptus.Seq_

// ===========================================================================
// until scala 2 macros are ready (t23112114209)
private object RuntimeInstantiatorCreator extends ReflectionTypesAbstraction {

  def fromFirstTypeArgFirstTypeArg[T: WTT]: Instantiator =
    runiverse.weakTypeTag[T].pype { wtt =>
      rec(wtt.mirror)(wtt.tpe.typeArgs.force.one.typeArgs.force.one) }

  // ---------------------------------------------------------------------------
  def fromFirstTypeArg[T: WTT]: Instantiator =
    runiverse.weakTypeTag[T].pype { wtt =>
      rec(wtt.mirror)(wtt.tpe.typeArgs.force.one) }

  // ---------------------------------------------------------------------------
  def fromTypeDirectly[T: WTT]: Instantiator =
    runiverse.weakTypeTag[T].pype { wtt =>
      rec(wtt.mirror)(wtt.tpe) }

  // ===========================================================================
  private def rec
          (mirror: runiverse.Mirror)
          (tpe   : runiverse.Type) // that we want to instantiate
        : Instantiator = {
      val tn: TypeNode = TypeLeafParserRuntime2._parseTypeNode(tpe)

      val nestedObjs: Map[String /* key */, Instantiator] =
        tn
          .leaf.fields
          .zipSameSize(ReflectUtils.fieldTpes(tpe))
          .flatMap { case (field, fieldTpe) =>
            if  (!field.typeNode.isContainedDataClass) None /* need no instantiation then */
            else (field.typeNode.containerType match {
                case Container._One => rec(mirror)(fieldTpe)
                case Container._Pes => rec(mirror)(fieldTpe.typeArgs.force.one.typeArgs.force.one)
                case Container._Opt => rec(mirror)(fieldTpe.typeArgs.force.one)
                case Container._Nes => rec(mirror)(fieldTpe.typeArgs.force.one) })
              .pipe { instantiator => Some(field.key -> instantiator) } }
          .force.map

      // ---------------------------------------------------------------------------
      new Instantiator(
        f = args =>
          mirror
            .runtimeClass(tpe)
            .getConstructors
            .headOption // t200720101733 - establish always safe or add corresponding validation
            .getOrElse(null) // TODO: handle better (happens with eg .removeIf('f).hasValue(None))
            .newInstance(args.map(_.asInstanceOf[Object] /* for scala 2.12 only */):_*),
        nestedObjs)
  }

}

// ===========================================================================
