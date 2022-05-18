package gallia

// ===========================================================================
package object target { // TODO: t210202090211 - p3 - rename package and homogenize with selection package...

  private[target] type TypeNode = gallia.reflect.TypeNode

  // ---------------------------------------------------------------------------
  private[target] type SubInfo   = gallia.meta.SubInfo
  private[target] type Info      = gallia.meta.Info
  private[target] type Info1     = gallia.meta.Info1
  private[target] type Fld       = gallia.meta.Fld

  private[target] type Container = gallia.reflect.Container
  private[target] val  Container = gallia.reflect.Container

  private[target] type TQ[$Target] = TargetQuery[$Target]
  
  private[target] val DataClassUtils = gallia.target.utils.DataClassUtils
  
  // ===========================================================================
  type TqKey    = TargetQuery[Key]
  type TQRen    = TargetQuery[Ren ]

  // ---------------------------------------------------------------------------
  type TqKeyz   = TargetQuery[Keyz]
  type TQRenz   = TargetQuery[Renz]

  // ---------------------------------------------------------------------------
  type TqKPath  = TargetQuery[KPath]
  type TqKPathz = TargetQuery[KPathz]

  // ---------------------------------------------------------------------------
  type TqRPath  = TargetQuery[RPath]
  type TqRPathz = TargetQuery[RPathz]

  // ---------------------------------------------------------------------------
  type TqKPath2  = TargetQuery2 [KPath]
  type TqKPath3  = TargetQuery3 [KPath]
  type TqKPath4  = TargetQuery4 [KPath]
  type TqKPath5  = TargetQuery5 [KPath]
  type TqKPath6  = TargetQuery6 [KPath]
  type TqKPath7  = TargetQuery7 [KPath]
  type TqKPath8  = TargetQuery8 [KPath]
  type TqKPath9  = TargetQuery9 [KPath]
  type TqKPath10 = TargetQuery10[KPath]

  // ===========================================================================
  // TODO: t210124100009 - find simpler shorthands for these two, pretty common; sigh, naming things...
  type TtqKPath  = TtqKPath1
  type TtqRPathz = TtqRPathz1

    // ---------------------------------------------------------------------------
    type TtqKPath1  = TypedTargetQuery[KPath]
    type TtqRPathz1 = TypedTargetQuery[RPathz]

    type TtqKPath2  = TypedTargetQuery2[KPath]
    type TtqRPathz2 = TypedTargetQuery2[RPathz]

    type TtqKPath3  = TypedTargetQuery3[KPath]
    type TtqRPathz3 = TypedTargetQuery3[RPathz]

    type TtqKPath4  = TypedTargetQuery4[KPath]
    type TtqRPathz4 = TypedTargetQuery4[RPathz]

    type TtqKPath5  = TypedTargetQuery5[KPath]
    type TtqRPathz5 = TypedTargetQuery5[RPathz]

    type TtqKPath6  = TypedTargetQuery6[KPath]
    type TtqRPathz6 = TypedTargetQuery6[RPathz]

    type TtqKPath7  = TypedTargetQuery7[KPath]
    type TtqRPathz7 = TypedTargetQuery7[RPathz]

    type TtqKPath8  = TypedTargetQuery8[KPath]
    type TtqRPathz8 = TypedTargetQuery8[RPathz]
  
    type TtqKPath9  = TypedTargetQuery9[KPath]
    type TtqRPathz9 = TypedTargetQuery9[RPathz]
    
    type TtqKPath10  = TypedTargetQuery10[KPath]
    type TtqRPathz10 = TypedTargetQuery10[RPathz]
  
  // ===========================================================================
  case class Duo[$Target](node: TypeNode, target: $Target) extends HasTypeNode // TODO: rename
  
  // ===========================================================================
  trait CanResolve[$Target] { val resolve: Cls => $Target } // for TargetQuery
  
  // ---------------------------------------------------------------------------
  trait CanValidateQuery { val vldtTargetQuery : Cls => Errs    }
  
  // ---------------------------------------------------------------------------
  trait HasTypeSeq { def hts: Seq[HasType] } 
}

// ===========================================================================
