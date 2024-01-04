package gallia
package trgt
package utils

import trgt._
import selection.untyped.processors._

// ===========================================================================
object TargetQueryValidation { //TODO: move
  import gallia.vldt.{MetaValidation => _vldt}

  // ---------------------------------------------------------------------------
  def tqkey   (sel: KeySelection   ) = new TqKey   (c => sel.vldt(c) ++ sel.rpathz(c).pipe(_vldt.fieldsRenaming(c, _)), sel.key )
  def tqkeyz  (sel: KeyzSelection  ) = new TqKeyz  (c => sel.vldt(c) ++ sel.rpathz(c).pipe(_vldt.fieldsRenaming(c, _)), sel.keyz)
  def tqren   (sel: RenSelection   ) = new TqRen   (c => sel.vldt(c) ++ sel.rpathz(c).pipe(_vldt.fieldsRenaming(c, _)), sel.ren )
  def tqrenz  (sel: RenzSelection  ) = new TqRenz  (c => sel.vldt(c) ++ sel.rpathz(c).pipe(_vldt.fieldsRenaming(c, _)), sel.renz)
  def tqkpath (sel: KPathSelection ) = new TqKPath (c => sel.vldt(c) ++ sel.rpathz(c).pipe(_vldt.fieldsRenaming(c, _)), sel.kpath)
  def tqkpathz(sel: KPathzSelection) = new TqKPathz(c => sel.vldt(c) ++ sel.rpathz(c).pipe(_vldt.fieldsRenaming(c, _)), sel.kpathz)
  def tqrpathz(sel: KPathzSelection) = new TqRPathz(c => sel.vldt(c) ++ sel.rpathz(c).pipe(_vldt.fieldsRenaming(c, _)), sel.rpathz)
  def tqrpathz(sel: RPathzSelection) = new TqRPathz(c => sel.vldt(c) ++ sel.rpathz(c).pipe(_vldt.fieldsRenaming(c, _)), sel.rpathz)
}

// ===========================================================================
