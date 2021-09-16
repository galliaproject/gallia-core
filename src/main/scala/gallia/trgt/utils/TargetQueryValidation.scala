package gallia.target.utils

import aptus.Anything_

import gallia.target._
import gallia.selection.untyped.processors._

// ===========================================================================
object TargetQueryValidation { //TODO: move
  import gallia.vldt.{MetaValidation => _vldt}

  // ---------------------------------------------------------------------------
  def tqkey   (sel: KeySelection   ) = new TqKey   (c => sel.vldt(c) ++ sel.qpathz(c).thn(_vldt.fieldsRenaming(c, _)), sel.key )
  def tqkeyz  (sel: KeyzSelection  ) = new TqKeyz  (c => sel.vldt(c) ++ sel.qpathz(c).thn(_vldt.fieldsRenaming(c, _)), sel.keyz)
  def tqren   (sel: RenSelection   ) = new TQRen   (c => sel.vldt(c) ++ sel.qpathz(c).thn(_vldt.fieldsRenaming(c, _)), sel.ren )
  def tqrenz  (sel: RenzSelection  ) = new TQRenz  (c => sel.vldt(c) ++ sel.qpathz(c).thn(_vldt.fieldsRenaming(c, _)), sel.renz)
  def tqkpath (sel: KPathSelection ) = new TqKPath (c => sel.vldt(c) ++ sel.qpathz(c).thn(_vldt.fieldsRenaming(c, _)), sel.kpath)
  def tqkpathz(sel: KPathzSelection) = new TqKPathz(c => sel.vldt(c) ++ sel.qpathz(c).thn(_vldt.fieldsRenaming(c, _)), sel.kpathz)
  def tqqpathz(sel: KPathzSelection) = new TqRPathz(c => sel.vldt(c) ++ sel.qpathz(c).thn(_vldt.fieldsRenaming(c, _)), sel.qpathz)
  def tqqpathz(sel: RPathzSelection) = new TqRPathz(c => sel.vldt(c) ++ sel.qpathz(c).thn(_vldt.fieldsRenaming(c, _)), sel.qpathz)
}

// ===========================================================================
