package gallia
package meta

import aptus._

// ===========================================================================
trait ClsAdvanced { self: Cls =>

  /** expects no conflicts                   */ def mergeDisjoint  (that: Cls): Cls = that.fields.foldLeft(this)(_ add _)
  /** expects common fields to be compatible */ def unionCompatible(that: Cls): Cls =
    Cls(
      fields
        .map { thisField =>
          that.field_(thisField.key) match {
            case None            => thisField.toOptional
            case Some(thatField) => Fld(thisField.key, Info.combine(thisField.info, thatField.info)) } } ++
      that
        .fields
        .flatMap { thatField =>
          field_(thatField.key) match {
            case None            => Some(thatField.toOptional)
            case Some(thatField) => None } } )

  // ===========================================================================
  def reorderKeys(f: Seq[Key] => Seq[Key], recursively: Boolean): Cls =
    copy(fields =
      keys
        .pipe(f)
        .map(field(_))
        .map { field =>
          if (!recursively) field
          else
            field.transformNestedClasses( // TODO: t220422133901 - classES ok?
              _.reorderKeys(f, recursively)) })

  // ===========================================================================
  def swapFields(parent: Option[RPath], key1: Key, key2: Key): Cls =
    parent match {
      case None    =>                             swapFields(key1, key2)
      case Some(p) => transformNestedClasses(p)(_.swapFields(key1, key2)) } // TODO: t220422133901 - classES ok?

  // ---------------------------------------------------------------------------
  def copyField(target: KPath, newKey: Key): Cls =
    transformx(target)(
        _.copyField(_, newKey),
        _.copyField(_, newKey))

  // ---------------------------------------------------------------------------
  def swapFields(key1: Key, key2: Key): Cls =
    fields
      .map { field =>
             if (field.key == key1) field.updateKey(key2)
        else if (field.key == key2) field.updateKey(key1)
        else                        field }
      .pipe(rewrap)

  // ---------------------------------------------------------------------------
  def copyField(key: Key, newKey: Key): Cls = rewrap(fields :+ field(key).updateKey(newKey))

  // ===========================================================================
  def zipStrings(keys: Renz, newNestingKey : Key): Cls =
      add(  key  = newNestingKey,
            info = Info.nes(nestedClass(keys)) )
        .remove(keys.froms)

    // ---------------------------------------------------------------------------
    private def nestedClass(keys: Renz): Cls =
      keys
        .values
        .map { entry =>
          field(entry.from)
            .updateKey(entry.to)
            .toSingle
            .toOptional } // TODO: t210108203819 offer a strict version
        .pipe(Cls.apply)

  // ===========================================================================
  def deserializez(targetKey: Ren, newKeys: Keyz): Cls = deserialize(targetKey, newKeys, _ => _Single)   // TODO: add check one string;
  def deserializea(targetKey: Ren, newKeys: Keyz): Cls = deserialize(targetKey, newKeys, identity)       // TODO: add check strings
  def deserializeb(targetKey: Ren, newKeys: Keyz): Cls = deserialize(targetKey, newKeys, _ => _Multiple) // TODO: can it be empty?

    // ---------------------------------------------------------------------------
    private def deserialize(targetKey: Ren, newKeys: Keyz, f: Multiple => Multiple): Cls =
      transformSoleSubInfo(targetKey) { // see 210109100250, may have to force them all to be strings
        subInfo => SubInfo(
            f(subInfo.multiple),
            newKeys
              .map(Fld(_, Info.oneString))
              .pipe(Cls.apply)) }

  // ===========================================================================
  def unarrayEntries(newKeys: Keyz, valueKey: Key): Cls = // TODO: rename to include "pivot"
      field(valueKey)
        .pipe { valueField =>
          newKeys
            .values
            .map(valueField.updateKey)
            .map(_.toOptional) // pivot keys can't be required unless explicitly set so
            .pipe(Cls.apply) }

    // ---------------------------------------------------------------------------
    @deprecated("see 210303104417") def unarrayBy0(keys: Keyz, newKeys: Keyz): Cls =
      remove(keys)
        .pipe { remaining =>
          newKeys
            .values
            .map(Fld(_, Info.one(remaining)))
            .pipe(Cls.apply) }

  // ===========================================================================
  def unpivot(keyz: Keyz): Cls = {
    val rest   = self.remove(keyz)
    val target = self.retain(keyz)
    
    // ---------------------------------------------------------------------------
    val value =
      target
        .fields
        .head // validated
        .pipe { field =>                      
          cls(
            Fld.oneString(_id),                      
            field.updateKey(_vle)) }

    rest.add(Fld.nesCls(_group, value))
  }

  // ===========================================================================
  def fuseToUnion(dest1: Key, dest2: Key)(union: Key): Cls =
      addBefore(
          field = union.requiredUnion(
            field(dest1).union ++
            field(dest2).union),
          target = dest1)
        .remove(dest1)
        .remove(dest2)

    // ---------------------------------------------------------------------------
    def fissionFromUnion(origin: Key)(dest1: Key, dest2: Key): Cls = {
      val (subInfo1, subInfo2) = field(origin).union.force.tuple2

       addBefore(Fld.optional(dest1, subInfo1), target = origin)
      .addAfter (Fld.optional(dest2, subInfo2), target = origin)
      .remove(origin)
    }

}

// ===========================================================================
