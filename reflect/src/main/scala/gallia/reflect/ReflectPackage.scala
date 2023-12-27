package gallia

// ===========================================================================
package object reflect {
  private[reflect] type EnumEntry = enumeratum.EnumEntry

  // ---------------------------------------------------------------------------
  private[reflect] implicit class Reflect_[A](value: A) { // so as to not import chaining._ everywhere
    private[reflect] def pipe[B](f: A => B)   : B =   f(value)
    private[reflect] def pype[B](f: A => B)   : B =   f(value) }

  // ---------------------------------------------------------------------------
  /** eg "java.lang.File" */          type FullNameString  = String // favor FullName now (items: Seq[String])
  /** eg "down" for Direction.down */ type EntryNameString = String }

// ===========================================================================
