package gallia.target.utils

import gallia._
import gallia.target._

// ===========================================================================
object TargetQueryUtils {

  // favor explicit selection now (via .explicit)
  def tqkpath (value: KPath)  = new TqKPath (_ => Nil, _ => value)
  def tqkpathz(value: KeyW)   = new TqKPathz(_ => Nil, _ => KPathz.from(value))
  def tqqpathz(value: RPathz) = new TqRPathz(_ => Nil, _ => value)
  def tqqpathz(value: RPathW) = new TqRPathz(_ => Nil, _ => value.qpathz)

  // ---------------------------------------------------------------------------
  def tqkpath2(k1: KPathW, k2: KPathW) = new TqKPath2(tqkpath(k1.value), tqkpath(k2.value))
}

// ===========================================================================
