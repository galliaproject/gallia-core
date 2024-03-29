package gallia
package vldt

import aptus.{String_, Seq_}
import aptus.{IdValue, Label}

import meta._

// ===========================================================================
@deprecated("TODO: t210121101206 - will be completely rehauled") object ErrorId {
    val CantBeEmpty = "201115104730" -> "CantBeEmpty"

    val InvalidKeyName = "201115104731" -> "InvalidKeyName"

    val FieldAlreadyExists = "201114161808" -> "FieldAlreadyExists"
    val NoSuchField        = "201114161904" -> "NoSuchField"

    val NoFieldsLeft = "201107145243" -> "NoFieldsLeft"
    val DuplicateKeys = "201114161455" -> "DuplicateKeys" // eg in add(f, f)
    val NotDisjoint = "201114161701" -> "NotDisjoint"

    @deprecated val TypeMismatch = "201101174017" -> "TypeMismatch"

    val CouldNotRenameDynamically = "201114101043" -> "CouldNotRenameDynamically"

    val NotNesting = "210109145953" -> "NotNesting"
    val NotNumeric = "210109145954" -> "NotNumeric"

    // ---------------------------------------------------------------------------
    // reflect
    val InvalidClassName = "201101144140" -> "InvalidClassName"
    val GenericClass     = "201101144141" -> "GenericClass"
    val NoFields         = "201101144142" -> "NoFields"

    // ---------------------------------------------------------------------------
    val UnsupportedTlSubtype = "201101145247" -> "UnsupportedTlSubtype"
    val InvalidTypeNode      = "201101145245" -> "InvalidTypeNode"

    // ---------------------------------------------------------------------------
    // selection
    @deprecated val MoreThanOneKey = "201110091500" -> "MoreThanOneKey"
    @deprecated val OutOfBoundKey  = "201110144638" -> "OutOfBoundKey"

    // ---------------------------------------------------------------------------
    @deprecated object Runtime {
      @deprecated val NotUnique            = "201110144609" -> "NotUnique"
      @deprecated val NoKeysLeft           = "201110144610" -> "NoKeysLeft" //TODO: or offer alterative if all missing?
      @deprecated val EmptyKey             = "201110144611" -> "EmptyKey"

      @deprecated val InvalidKey = "201101144150" -> "InvalidKey"
    }
  }

  // ===========================================================================
  @deprecated("will be completely rehauled") trait _HasCompanion1 {
      val companion: _ErrorCompanion

      val errorId = companion.errorId;
      val label   = companion.label }

    // ---------------------------------------------------------------------------
    abstract class _HasCompanion2(companion: _ErrorCompanion) {
        final val errorId = companion.errorId;
        final val label   = companion.label }

      // ---------------------------------------------------------------------------
      abstract class _ErrorCompanion(
        val errorId: IdValue, // eg "201009114800"
        val label  : Label )  // eg "SomethingWrong"

  // ===========================================================================
  @deprecated("t210121101206 - will be completely rehauled") /*sealed */trait _Error {
      val errorId: IdValue // eg "201009114800"
      val label  : Label   // eg "SomethingWrong"

      // ---------------------------------------------------------------------------
      def formatDetails2: Option[String]

      // ---------------------------------------------------------------------------
      def err  = Err(s"${errorId} - ${label}".append(formatDetails2.map(_.prepend(": ")).getOrElse("")))
      def errs = Seq(err)
      def err_ = Some(err)

      def errIf (test: => Boolean): Err_ = if (test) err_ else None
      def errsIf(test: => Boolean): Errs = if (test) errs else Nil

      // TODO: delay throwing?
      def throwDataError[A](passThrough: A): A       = { dataError((errorId, label, formatDetails2)); passThrough; }
      def throwDataError   ()              : Nothing = { dataError((errorId, label, formatDetails2)) }
    }

    // ---------------------------------------------------------------------------
    sealed trait _Error1 extends _Error { final def formatDetails2 = Some(formatDetails); def formatDetails: String }
    sealed trait _Error2 extends _Error { final def formatDetails2 = None }
    sealed trait _Error3 extends _Error { final def formatDetails2 = Some(toString.stripPrefix(getClass.toString/*.getSimpleName - java.lang.InternalError: Malformed class name*/)) }
    sealed trait _Error4[T] extends _Error3 {
      val target: T
      final def errIf (p: T => Boolean): Err_ = errIf(p(target))
      final def errsIf(p: T => Boolean): Errs = errIf(p(target)).toSeq }

    // ===========================================================================
    @deprecated("will be completely rehauled") object _Error { // t201120101734 - reformat error(s)
        // TODO: macro annotations to help with boilerplate

      case object ObjCantBeEmpty               extends _Error2 { val errorId = "210113121804"; val label = "ObjCantBeEmpty" }
      case class  ObjDuplicateKeys(keys: Keyz) extends _Error3 { val errorId = "201026170344"; val label = "ObjDuplicateKeys" }
      
      case object CantBeNone extends _Error2 { val errorId = "201115104732"; val label = "CantBeNone" }

      // ---------------------------------------------------------------------------
      case object MetaAssertionFailure extends _Error3 { val errorId = "201014110606"; val label = "MetaAssertionFailure" }

      case class FieldAssertionFailure    (target: KPath) extends _Error3 { val errorId = "201014112650"; val label = "FieldAssertionFailure" }
      case class ContainerAssertionFailure(target: KPath, container: Container) extends _Error3 { val errorId = "201014112651"; val label = "ContainerAssertionFailure" }
      case class ValueTypeAssertionFailure(target: KPath, basicType: BasicType) extends _Error3 { val errorId = "201014112652"; val label = "ValueTypeAssertionFailure" }

      // ===========================================================================
      case class InvalidKeyReordering(origin: Seq[SKey], destination: Seq[SKey]) extends _Error3 { val errorId = "201019112159"; val label = "InvalidKeyReordering" }

      // ---------------------------------------------------------------------------
      case class Tmp (target : KPath , msg: String) extends _Error3 { val errorId = "201016153347"; val label = "TODO:201016153347" }

      // ---------------------------------------------------------------------------
      // TODO: distinguish container/valueType error
      object       TypeMismatch extends _ErrorCompanion("201101174017", "TypeMismatch")
        case class TypeMismatch(kpath: KPath, infoA: Info, info1B: Info1, mode: SpecialCardiMode) extends _HasCompanion2(
                   TypeMismatch) with  _Error1 {
          def formatDetails =
            (Seq(
                s"for ${kpath}:",
                s"\texpected:\t${infoA .formatDefault},", // see t210125111338 (union types)
                 s"\tbut got:\t${info1B.formatDefault}") ++
               (if (mode.isNormal) Nil else Seq(s"(mode: ${mode})")))
              .joinln }

      // ---------------------------------------------------------------------------
      case class InvalidDataClass(details: Any)       extends _Error3 { val errorId = "201015101701"; val label = "InvalidDataClass" }
      case class SchemaMismatch(cls1: Cls, cls2: Cls) extends _Error3 { val errorId = "201015101700"; val label = "SchemaMismatch" }

      // ---------------------------------------------------------------------------
      case class OutOfBoundKey(maxSize: Int, offendingIndices: Seq[Int]) extends _Error1 {
        val errorId = "201110144638"; val label = "OutOfBoundKey"
          def formatDetails = offendingIndices.#@@.append(s"(${maxSize})") }

      // ---------------------------------------------------------------------------
      case class MoreThanOneKey(keys: Seq[Key]) extends _Error1 { // eg when trying to use "sole" key selector
        val errorId = "201110091500"; val label = "MoreThanOneKey"
          def formatDetails = keys.#@@ }

      // ---------------------------------------------------------------------------
      case class AmbiguousMergingKey (keys: Keyz) extends _Error3 { val errorId = "220207150612"; val label = "AmbiguousMergingKey" }
      case class NameConflictsForJoin(keys: Keyz) extends _Error3 { val errorId = "220207150613"; val label = "NameConflictsForJoin" } // t220209085836 - name conflict in join: offer mode to discard RHS conflicts, and mode to rename RHS      

      // ---------------------------------------------------------------------------
      case class NotAnEnumField         (kpath: KPath)                 extends _Error3 { val errorId = "220504143050"; val label = "NotAnEnumField" }
      case class InvalidEnumStringValues(values: Seq[EnumStringValue]) extends _Error3 { val errorId = "220504143051"; val label = "InvalidEnumStringValues" }

      // ---------------------------------------------------------------------------
      case class NotAnUnionField   (kpath: KPath) extends _Error3      { val errorId = "220511144415"; val label = "NotAnUnionField" }
      case class MoreThanOneNesting(target: Fld)  extends _Error4[Fld] { val errorId = "220511152109"; val label = "MoreThanOneNesting" }

      // ---------------------------------------------------------------------------
      case class NotExactlyOneNestedClass(target: Fld) extends _Error3 { val errorId = "220517100834"; val label = "NotExactlyOneNestedClass" }

      // ---------------------------------------------------------------------------
      case class SchemaMustNotChange(c1: Cls, c2: Cls) extends _Error3 { val errorId = "220518124640"; val label = "SchemaMustNotChange" }

      // ===========================================================================
      object Runtime {

        case class NotDefined(targets: KPathz) extends _Error3 { val errorId = "201016153348"; val label = s"NotDefined"
          def formatDetails = targets.formatDefault }

        // ---------------------------------------------------------------------------
        case class NotUnique(keysOpt: Option[Keyz] /* None is All */, sizes: (Int, Int)) extends _Error2 { val errorId = "201110144609"; val label = "NotUnique" }

        // ---------------------------------------------------------------------------
        //TODO: delay throwing to accumulate?
        case object DataUnsafeUAssertionFailure extends _Error2 { val errorId = "201014121746"; val label = "DataUnsafeUAssertionFailure"
            def runtimeIfU(o: Obj)(invalid: Obj => Boolean): Obj =
              if (invalid(o)) dataError((errorId, label)) else o }

          // ---------------------------------------------------------------------------
          case object DataUnsafeZAssertionFailure extends _Error2 { val errorId = "201014121748"; val label = "DataUnsafeZAssertionFailure"
            def runtimeIfZ(z: Objs)(invalid: Objs => Boolean): Objs =
              if (invalid(z)) dataError((errorId, label)) else z }

        // ---------------------------------------------------------------------------
        case class DataAssertionFailure(path1: Any) extends _Error3 { val errorId = "201014121747"; val label = "DataAssertionFailure"
          def attemptO(o: Obj)(invalid: Obj => Boolean): Obj =
            if (invalid(o)) dataError((errorId, label)) else o }

        // ---------------------------------------------------------------------------
        case object EmptyStream               extends _Error2 { val errorId = "201110144607"; val label = s"EmptyStream" }
        case class  MoreThanNElements(n: Int) extends _Error3 { val errorId = "201110144606"; val label = s"MoreThanNElements"
          def formatDetails = s"n:${n}"} // note: actual size could be costly to compute
        
        // ---------------------------------------------------------------------------
        case class DifferingRuntimeType(from: String, to: String) extends _Error3 { val errorId = "210811104025"; val label = "DifferingRuntimeType" }
        
        case class WhateverOperationForbidden(error: String) extends _Error3 { val errorId = "210811145146"; val label = "WhateverOperationForbidden" }

        case class NotAPartition(keys: Keyz) extends _Error3 { val errorId = "220517094323"; val label = "NotAPartition" }
      }
    }

// ===========================================================================
