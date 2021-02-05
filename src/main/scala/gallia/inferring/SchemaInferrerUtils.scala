package gallia.inferring

import aptus.{Anything_, Seq_}
import aptus.Nes

import gallia._
import gallia.meta.{Cls => _, Fld => _, _}

// ===========================================================================
private object SchemaInferrerUtils {
  class IncompatibleInfoException(pairs: Seq[FldPair]) extends Exception {
    override def getMessage: String = pairs.map(_.formatDefault).section2 }

  // ===========================================================================
  implicit class Cls_(dis: Cls) {

    def combine(that: Cls): Cls =
        dis
          .fields
          .map { existingField =>
            that.field_(existingField.key)
              .map { newField =>
                if (Fld.isIntAndDouble(existingField, newField)) existingField.toDouble
                else                                  existingField }
              .getOrElse(existingField) }
          .thn(Cls.apply)
          .sideEffect {
            _ .differingInfos(that)
              .foreach { pairs => throw new IncompatibleInfoException(pairs) } }
          .reviseRequirednessBasedOn(that) // not in that
          .addMissingFieldsFrom     (that) //     in that

    // ===========================================================================
    def reviseRequirednessBasedOn(that: Cls): Cls =
      dis.keys.diff(that.keys)
        .map(dis.field(_))
        .map(_.toNonRequired) // because not present in that, therefore not required
        .foldLeft(dis) { (curr, field) =>
          curr.replace(field.key, field.info) }

    // ---------------------------------------------------------------------------
    def addMissingFieldsFrom(that: Cls): Cls =
      that.keys.diff(dis.keys)
        .map(that.field(_))
        .map(_.toNonRequired) // because not present in this, therefore not required
        .foldLeft(dis)(_ add _)

    // ---------------------------------------------------------------------------
    def differingInfos(that: Cls): Option[Nes[FldPair]] =
      dis.keys.intersect(that.keys)
        .flatMap { key =>
          val existingField = dis .field(key)
          val newField      = that.field(key)

          if (newField.info != existingField.info)
            if (Fld.isIntAndDouble(newField, existingField)) None
            else                                             Some(FldPair(existingField, newField))
          else                                               None }
        .as.noneIf(_.isEmpty)
  }

}

// ===========================================================================
