package gallia.vldt

import aptus.{String_, Seq_}

import aptus.IdValue
import aptus.Label

import gallia._
import gallia.meta._

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
      def err = Err(s"${errorId} - ${label}".append(formatDetails2.map(_.prepend(": ")).getOrElse("")))
      def errs = Seq(err)
      def err_ = Some(err)

      def errIf (test: => Boolean): gallia.Err_ = if (test) err_ else None
      def errsIf(test: => Boolean): gallia.Errs = if (test) errs else Nil

      // TODO: delay throwing?
      def throwRuntimeError[A](passThrough: A): A       = { dataError((errorId, label, formatDetails2)); passThrough; }
      def throwRuntimeError   ()              : Nothing = { dataError((errorId, label, formatDetails2)) }
    }

    // ---------------------------------------------------------------------------
    sealed trait _Error1 extends _Error { final def formatDetails2 = Some(formatDetails); def formatDetails: String }
    sealed trait _Error2 extends _Error { final def formatDetails2 = None }
    sealed trait _Error3 extends _Error { final def formatDetails2 = Some(toString.stripPrefix(getClass.toString/*.getSimpleName - java.lang.InternalError: Malformed class name*/)) }

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
      case class ContaineeAssertionFailure(target: KPath, basicType: BasicType) extends _Error3 { val errorId = "201014112652"; val label = "ContaineeAssertionFailure" }

      // ===========================================================================
      case class InvalidKeyReordering(origin: Seq[SKey], destination: Seq[SKey]) extends _Error3 { val errorId = "201019112159"; val label = "InvalidKeyReordering" }

      // ---------------------------------------------------------------------------
      case class Tmp (target : KPath , msg: String) extends _Error3 { val errorId = "201016153347"; val label = "TODO:201016153347" }

      // ---------------------------------------------------------------------------
      // TODO: distinguish container/containee error
      object       TypeMismatch extends _ErrorCompanion("201101174017", "TypeMismatch")
        case class TypeMismatch(kpath: KPath, infoA: Info, infoB: Info, mode: SpecialCardiMode) extends _HasCompanion2(
                   TypeMismatch) with  _Error1 {
          def formatDetails =
            Seq(
                s"for ${kpath}:",
                s"expected: ${infoA.formatDefault(":")},",
                 s"but got: ${infoB.formatDefault(":")}",
                s"(mode: ${mode})")
              .joinln }

      // ---------------------------------------------------------------------------
      case class InvalidDataClass(details: Any)       extends _Error3 { val errorId = "201015101701"; val label = "InvalidDataClass" }
      case class SchemaMismatch(cls1: Cls, cls2: Cls) extends _Error3 { val errorId = "201015101700"; val label = "SchemaMismatch" }

      // ---------------------------------------------------------------------------
      case class OutOfBoundKey(maxSize: Int, offendingIndices: Seq[Int]) extends _Error1 {
        val errorId = "201110144638"; val label = "OutOfBoundKey"
          def formatDetails = offendingIndices.#@@.append(s"(${maxSize})") }

      // ---------------------------------------------------------------------------
      case class MoreThanOneKey(keys: Seq[gallia.Key]) extends _Error1 {
        val errorId = "201110091500"; val label = "MoreThanOneKey"
          def formatDetails = keys.#@@ }

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
              if (invalid(o)) runtimeError((errorId, label)) else o }

          // ---------------------------------------------------------------------------
          case object DataUnsafeZAssertionFailure extends _Error2 { val errorId = "201014121748"; val label = "DataUnsafeZAssertionFailure"
            def runtimeIfZ(z: Objs)(invalid: Objs => Boolean): Objs =
              if (invalid(z)) runtimeError((errorId, label)) else z }

        // ---------------------------------------------------------------------------
        case class DataAssertionFailure(path1: Any) extends _Error3 { val errorId = "201014121747"; val label = "DataAssertionFailure"
          def attemptO(o: Obj)(invalid: Obj => Boolean): Obj =
            if (invalid(o)) runtimeError((errorId, label)) else o }

        case object EmptyStream               extends _Error2 { val errorId = "201110144607"; val label = s"EmptyStream" }
        case class  MoreThanNElements(n: Int) extends _Error3 { val errorId = "201110144606"; val label = s"MoreThanNElements"
          def formatDetails = s"n:${n}"} // note: actual size could be costly to compute
        
        
      }
    }

// ===========================================================================
