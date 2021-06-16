package gallia.atoms

import scala.util.chaining._

import gallia._
import gallia.plans._

// ===========================================================================
case class _UWrapper(wrappee: AtomUU) extends AtomZZ { def naive(z: Objs) = z.map(wrappee.naive) } // TODO: see t210114111539

// ===========================================================================
case class _UWrappers(wrappees: Seq[AtomUU]) extends AtomZZ with AtomCombiner[_UWrapper] {  
  override def formatSuccinct1: String = s"${className} (${wrappees.size}), first: ${wrappees.headOption.map(_.formatSuccinct1)}"
  def naive(z: Objs) = z.map { initO => wrappees.foldLeft(initO)((currO, wrappee) => wrappee.naive(currO)) } }

  // ===========================================================================
  object _UWrappers {  
  
    def fromMapU2U(plan: AtomPlan): _UWrappers = 
      plan
        .dag
        .nodes // TODO: t210614142629 - confirm/enforce guaranteed topologically sorted if chain?
        .tail
        .pipe(AtomNodes.apply)
        .pruneChain
        .values
        .map (_.atom.asInstanceOf[AtomUU])
        .pipe(_UWrappers.apply)       
  
    // ---------------------------------------------------------------------------
    def from(values: Seq[_UWrapper]): _UWrappers =
      values  
          .map(_.wrappee)
          .pipe(AtomNodes.combineUWrapping)
          .map(_.asInstanceOf[AtomUU])  
        .pipe(_UWrappers.apply)  
      
  }
  
// ===========================================================================
