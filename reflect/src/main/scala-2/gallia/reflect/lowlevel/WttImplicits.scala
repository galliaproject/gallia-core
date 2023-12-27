package gallia
package reflect
package lowlevel

// ===========================================================================
trait WttImplicits { x: ReflectionTypesAbstraction =>

  // ---------------------------------------------------------------------------
  /** only for scala 2... */
  implicit class WeakTypeTag_[T: WTT](wtt: WTT[T]) {
    def typeNode       : TypeNode                     = TypeLeafParserRuntime2.parseTypeNode[T]
    def instantiatorOpt: Option[reflect.Instantiator] = Some(runtimeInstantiator[T](to = typeNode)) }

  // ===========================================================================
  // until macro versions are ready
  private def runtimeInstantiator[T: WTT](to: TypeNode): Instantiator = {
      if(!to.isContainedDataClass && !to.isContainedEnumeratum) null //FIXME
      else if(!to.isContainedDataClass)
        Instantiator.enumeratumWithName[String](
          f = x => CompanionReflection[T](methodName = "withName")(x))
      else if (to.isOptionOfSeq)        RuntimeInstantiatorCreator.fromFirstTypeArgFirstTypeArg[T] /* eg Option[Seq[MyCc]] */
      else if (to.isSeq || to.isOption) RuntimeInstantiatorCreator.fromFirstTypeArg            [T] /* eg Option[    MyCc ] */
      else                              RuntimeInstantiatorCreator.fromTypeDirectly            [T] /* eg            MyCc   */ } }

// ===========================================================================