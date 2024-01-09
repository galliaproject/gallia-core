package gallia
package inferring

import aptus._

import meta.{Cls => _, Fld => _, _}

// ===========================================================================
private object SchemaInferrerUtils {
  class IncompatibleInfoException(pairs: Seq[FldPair]) extends Exception {
    override def getMessage: String = pairs.map(_.formatDefault).section2(pairs.size.str) }

  // ===========================================================================
  implicit class Cls_(dis: Cls) {

    def combine(that: Cls): Cls = {
      val conflictingPairs: Seq[FldPair] = this.conflictingPairs(that)
      
      if (conflictingPairs.nonEmpty) { // 210802094043
        throw new IncompatibleInfoException(conflictingPairs) }

      dis
        .fields
        .map { existingField =>            
          that
            .field_(existingField.key)
            .map { newField =>                      
              resolve(existingField, newField).force /* else would error at above (see 210802094043) */ }
            .getOrElse(existingField) }
        .pipe(Cls.apply)
        .reviseRequirednessBasedOn(that) // not in that
        .addMissingFieldsFrom     (that) //     in that
    }
    
    // ---------------------------------------------------------------------------  
    private def conflictingPairs(that: Cls): Seq[FldPair] =
      dis
        .fields
        .flatMap { existingField =>            
          that
            .field_(existingField.key)
            .flatMap { newField =>
              resolve(existingField, newField)
                .swap(FldPair(existingField, newField)) } }

    // ===========================================================================
    def reviseRequirednessBasedOn(that: Cls): Cls =
      dis.keys.diff(that.keys)
        .map(dis.field(_))
        .map(_.toOptional) // because not present in that, therefore not required
        .foldLeft(dis) { (curr, field) =>
          curr.replace(field.key, field.info) }

    // ---------------------------------------------------------------------------
    def addMissingFieldsFrom(that: Cls): Cls =
      that.keys.diff(dis.keys)
        .map(that.field(_))
        .map(_.toOptional) // because not present in this, therefore not required
        .foldLeft(dis)(_ add _)
  }

  // ===========================================================================
  private def resolve(existingField: Fld, newField: Fld): Option[Fld] =
      (existingField.nestedClassOpt, newField.nestedClassOpt).toOptionalTuple match {
        case Some((e, n)) => e.combine(n).pipe(existingField.updateSoleNestedClass).in.some
        case None =>
          /**/ if (newField.subInfo1 == existingField.subInfo1)       Some(existingField)

          // note: not so for multiplicity, as it requires a data change (TODO: t210802090946 - reconsider)
          else if ( existingField.isRequired && !newField.isRequired) Some(existingField.toOptional)
          else if (!existingField.isRequired &&  newField.isRequired) Some(existingField)

          else if (areBoundedNumbers(existingField, newField))        Some(existingField.toDouble)

          else                                                        None }

    // ---------------------------------------------------------------------------
    private def areBoundedNumbers(f1: Fld, f2: Fld): Boolean =
      f1.basicTypeOpt.exists(_.isBoundedNumber) &&
      f2.basicTypeOpt.exists(_.isBoundedNumber)
}

// ===========================================================================
