package gallia

import aptus.Class_

// ===========================================================================
package object reflect {
  type UType = scala.reflect.api.Universe#Type

  // ---------------------------------------------------------------------------
  /** eg "java.lang.String" */ type FullName = String
  /** eg "String"           */ type Alias    = String

  // ===========================================================================
  private[reflect] val _AObj    = classOf[gallia.AObj].fullPath
  private[reflect] val _BObj    = classOf[gallia.BObj].fullPath

  private[reflect] val _HeadU    = classOf[gallia.heads.HeadU].fullPath
  private[reflect] val _HeadZ    = classOf[gallia.heads.HeadZ].fullPath

  // ===========================================================================
  private[reflect] val _Seq    = classOf[scala.collection.Seq[_]].fullPath
  private[reflect] val _Option = classOf[scala.Option[_]]        .fullPath

  private[reflect] val _Some   = classOf[scala.Some[_]].fullPath
  private[reflect] val _None   = scala.None.getClass.fullPath

  // ===========================================================================
  private[reflect] val _ByteBuffer = classOf[java.nio.ByteBuffer].fullPath

  // ===========================================================================
  private[reflect] val _EnumEntry = classOf[enumeratum.EnumEntry].fullPath
  private[gallia]  val _EnumValue = classOf[gallia.EnumValue]    .fullPath
}

// ===========================================================================
