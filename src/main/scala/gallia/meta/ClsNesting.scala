package gallia.meta

import gallia._

// ===========================================================================
trait ClsNesting { dis: Cls =>

  private[meta /*cls*/] def transformx(path: KPath)(
       root: (Cls, Key  ) => Cls,
       rec : (Cls, KPath) => Cls)
     : Cls =
    path.tailPair match {
        case (leaf  , None      ) => root(dis, leaf)
        case (parent, Some(tail)) => dis.transformInfo(parent){
          _.transformNestedClass{ nc =>
              rec(nc, tail) } } }

  // ---------------------------------------------------------------------------
  private[meta /*cls*/] def transformx(path: RPath)(
       root: (Cls, Ren  ) => Cls,
       rec : (Cls, RPath) => Cls)
     : Cls =
    path.tailPair match {
        case Left ( renaming      ) => root(dis, renaming)
        case Right((parent, qpath)) => dis.transformInfo(parent){
          _.transformNestedClass{ nc =>
              rec(nc, qpath) } } }

}

// ===========================================================================
