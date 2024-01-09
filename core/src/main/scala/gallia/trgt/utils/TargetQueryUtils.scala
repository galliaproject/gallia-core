package gallia
package trgt
package utils

import trgt._

// ===========================================================================
object TargetQueryUtils {

  // favor explicit selection now (via .explicit)
  def tqkpath (value: KPath)  = new TqKPath (_ => Nil, _ => value)
  def tqkpathz(value: KeyW)   = new TqKPathz(_ => Nil, _ => KPathz.from(value))
  def tqrpathz(value: RPathz) = new TqRPathz(_ => Nil, _ => value)
  def tqrpathz(value: RPathW) = new TqRPathz(_ => Nil, _ => value.rpathz)

  // ---------------------------------------------------------------------------
  def tqkpath2(k1: KPathW, k2: KPathW)             = new TqKPath2(tqkpath(k1.value), tqkpath(k2.value))
  def tqkpath3(k1: KPathW, k2: KPathW, k3: KPathW) = new TqKPath3(tqkpath(k1.value), tqkpath(k2.value), tqkpath(k3.value))
}

// ===========================================================================
