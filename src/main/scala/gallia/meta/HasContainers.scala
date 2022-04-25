package gallia
package meta

import reflect.Container._

// ===========================================================================
trait HasContainer1 {
    protected val _container1: Container

    // ---------------------------------------------------------------------------
    def isContainer(value: Container): Boolean = _container1 == value

      def isOne: Boolean = isContainer(_One)
      def isOpt: Boolean = isContainer(_Opt)

      def isNes: Boolean = isContainer(_Nes)
      def isPes: Boolean = isContainer(_Pes)

      // ---------------------------------------------------------------------------
      // see t210125111338 (union types)
      def isSingle  : Boolean = _container1.isSingle
      def isMultiple: Boolean = _container1.isMultiple
      def isRequired: Boolean = _container1.isRequired
      def isOptional: Boolean = _container1.isOptional
  }

  // ===========================================================================
  trait HasContainers {
    protected val _containers: Seq[Container] // see t210125111338 (union types)

    // ---------------------------------------------------------------------------
    def isContainer(value: Container): Boolean = _containers.forall(_ == value) // see t210125111338 (union types)

      def isOne: Boolean = isContainer(_One)
      def isOpt: Boolean = isContainer(_Opt)

      def isNes: Boolean = isContainer(_Nes)
      def isPes: Boolean = isContainer(_Pes)

      // ---------------------------------------------------------------------------
      // see t210125111338 (union types)
      def isSingle  : Boolean = _containers.forall(_.isSingle)
      def isMultiple: Boolean = _containers.forall(_.isMultiple)
      def isRequired: Boolean = _containers.forall(_.isRequired)
      def isOptional: Boolean = _containers.forall(_.isOptional)
  }

// ===========================================================================
