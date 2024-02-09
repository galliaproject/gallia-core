package gallia
package result

import plans.{ActionPlan => Plan}

// ===========================================================================
class SuccessResult[$Data](
      val meta: SuccessMetaResult,
      val data: $Data) {
    def pair: (Cls, $Data) = (meta.forceLeafClass, data) }

  // ===========================================================================
  case class SuccessResultM   (override val meta: SuccessMetaResult, override val data: Plan) extends SuccessResult[Plan](meta, data) { def value: Plan  = data }
  case class SuccessResultV[T](override val meta: SuccessMetaResult, override val data: T)    extends SuccessResult[T]   (meta, data) { def value: T     = data }
  case class SuccessResultU   (override val meta: SuccessMetaResult, override val data: Obj ) extends SuccessResult[Obj] (meta, data) { def aobj : AObj  = AObj (meta.forceLeafClass, data) }
  case class SuccessResultZ   (override val meta: SuccessMetaResult, override val data: Objs) extends SuccessResult[Objs](meta, data) { def aobjs: AObjs = AObjs(meta.forceLeafClass, data._toViewBased) }

// ===========================================================================
