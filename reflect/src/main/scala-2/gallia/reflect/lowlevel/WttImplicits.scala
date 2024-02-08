package gallia
package reflect
package lowlevel

import scala.reflect.ClassTag

// ===========================================================================
trait WttImplicits { x: ReflectionTypesAbstraction =>

  // ---------------------------------------------------------------------------
  /** only for scala 2... */
  implicit class WeakTypeTag_[T: WTT](wtt: WTT[T]) {
    def typeNode       : TypeNode             = TypeLeafParserRuntime2.parseTypeNode[T]
    def ctag           : ClassTag[T]          = runtimeClassTag[T]
    def instantiatorOpt: Option[Instantiator] = Some(runtimeInstantiator[T](to = typeNode))

    def instantiator(implicit ev: T <:< enumeratum.EnumEntry): Instantiator = instantiatorOpt.get /* guaranteed by design for EnumEntry */ }

  // ===========================================================================
  // until macro versions are ready
  private def runtimeInstantiator[T: WTT](to: TypeNode): Instantiator = {
      if(!to.isContainedDataClass && !to.isContainedEnumeratum) null //FIXME
      else if(!to.isContainedDataClass)
        Instantiator.enumeratumWithName[String](
          f = x => CompanionReflection[T](methodName = "withName")(x))
      else if (to.isOptionOfSeq)        RuntimeInstantiatorCreator.fromFirstTypeArgFirstTypeArg[T] /* eg Option[Seq[MyCc]] */
      else if (to.isSeq || to.isOption) RuntimeInstantiatorCreator.fromFirstTypeArg            [T] /* eg Option[    MyCc ] */
      else                              RuntimeInstantiatorCreator.fromTypeDirectly            [T] /* eg            MyCc   */ }

  // ===========================================================================
  // see https://stackoverflow.com/questions/18729321/how-to-get-classtag-form-typetag-or-both-at-same-time
  private def runtimeClassTag[T : WTT]: ClassTag[T] = {
    val tag = runiverse.weakTypeTag[T]
    ClassTag[T](
      tag.mirror.runtimeClass(tag.tpe)) } }

// ===========================================================================
