package gallia

// ===========================================================================
package object oswo {
  trait OptimRunner { def run(): Any }

  // ---------------------------------------------------------------------------
  type AtomNodeId = String
  type ANI        = AtomNodeId

  type IsLast = Boolean

  type SourceString = String

  // ---------------------------------------------------------------------------
  type BasicType = meta.basic.BasicType
  val  BasicType = meta.basic.BasicType

  // ---------------------------------------------------------------------------
  private[oswo] def fullName(klass: Class[_]): String = klass.getName.stripSuffix("$").replace(".package$", ".")

  // ===========================================================================
  type _InMemoryInputUb  = atoms.AtomsIX._InMemoryInputUb
  type _JsonObjectString = atoms.AtomsIX._JsonObjectString

  // ---------------------------------------------------------------------------
  type _Rename           = atoms.common.AtomsCommonVeryBasics._Rename
  type _Add              = atoms.common.AtomsCommonVeryBasics._Add
//type _Replace          = atoms.common.AtomsCommonVeryBasics._Replace
  type _Remove           = atoms.common.AtomsCommonVeryBasics._Remove

  type _TransformVV      = atoms.common.AtomsCommonTransforms._TransformVV

  type _ConvertToDouble  = atoms.common.AtomsCommonConverts  ._ConvertToDouble

  // ===========================================================================
  case class OswoCtx(fromOpt: Option[ANI], id: ANI, last: IsLast) {
    def first: Boolean = fromOpt.isEmpty
    def from : ANI     = fromOpt.get }

}

// ===========================================================================
