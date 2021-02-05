package gallia.result

import gallia._
import gallia.domain.{AObj, AObjs}

// ===========================================================================
trait SuccessResult[$Data] {
    def meta: SuccessMetaResult
    def data: $Data
  }

  // ===========================================================================
  case class SuccessResultV[T](meta: SuccessMetaResult, data: T   ) extends SuccessResult[T   ] { def value: T     = data }
  case class SuccessResultU   (meta: SuccessMetaResult, data: Obj ) extends SuccessResult[Obj ] { def aobj : AObj  = AObj (meta.forceLeafClass, data) }
  case class SuccessResultZ   (meta: SuccessMetaResult, data: Objs) extends SuccessResult[Objs] { def aobjs: AObjs = AObjs(meta.forceLeafClass, data) }

// ===========================================================================
