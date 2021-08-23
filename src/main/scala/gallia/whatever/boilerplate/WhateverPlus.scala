package gallia.whatever.boilerplate

// ===========================================================================
object WhateverPlus {

  /* keep in sync with other boilerplate's Whatever{Plus,Times,...} */
  @gallia.NumberAbstraction 
  private[gallia] def apply(first: Any, second: Any): Any = {     
    first match { 
      
      // ---------------------------------------------------------------------------
      case x: Int => second match {
        case y: Int        => (x + y): Int
        case y: Double     => (x + y): Double
        
        case y: Byte       => (x + y): Int
        case y: Short      => (x + y): Int
        case y: Long       => (x + y): Long

        case y: Float      => (x + y): Float

        case y: BigInt     => (x + y): BigInt
        case y: BigDecimal => (x + y): BigDecimal }
      
      // ---------------------------------------------------------------------------
      case x: Double => second match {
        case y: Int        => (x + y): Double
        case y: Double     => (x + y): Double
        
        case y: BigInt     => (BigDecimal(x) + BigDecimal(y)): BigDecimal
        case y: BigDecimal => (x + y)                        : BigDecimal
        
        case y: Number     => (x + y.doubleValue()): Double }

      // ---------------------------------------------------------------------------
      case x: Byte => second match {
        case y: Int        => (x + y): Int
        case y: Double     => (x + y): Double
        
        case y: Byte       => (x + y): Int // see 210809113923
        case y: Short      => (x + y): Int // see 210809113923
        case y: Long       => (x + y): Long

        case y: Float      => (x + y): Float

        case y: BigInt     => (x + y): BigInt
        case y: BigDecimal => (x + y): BigDecimal }
      
      // ---------------------------------------------------------------------------
      case x: Short => second match {
        case y: Int        => (x + y): Int
        case y: Double     => (x + y): Double
        
        case y: Byte       => (x + y): Int // see 210809113923
        case y: Short      => (x + y): Int // see 210809113923
        case y: Long       => (x + y): Long

        case y: Float      => (x + y): Float

        case y: BigInt     => (x + y): BigInt
        case y: BigDecimal => (x + y): BigDecimal }
      
      // ---------------------------------------------------------------------------
      case x: Long => second match {
        case y: Double     => (x + y)             : Double
        case y: Float      => (x + y)             : Float

        case y: BigInt     => (BigInt(x) + y)     : BigInt
        case y: BigDecimal => (x + y)             : BigDecimal
        
        case y: Number     => (x + y.floatValue()): Float }
      
      // ---------------------------------------------------------------------------
      case x: Float => second match {
        case y: Double     => (x + y): Double

        case y: BigInt     => (BigDecimal(x) + BigDecimal(y)): BigDecimal
        case y: BigDecimal => (x + y)                        : BigDecimal
        
        case y: Number     => (x + y.floatValue()): Float }

      // ---------------------------------------------------------------------------
      case x: BigInt => second match {          
        case y: Int        => (x + y): BigInt          
        case y: Byte       => (x + y): BigInt
        case y: Short      => (x + y): BigInt
        case y: Long       => (x + y): BigInt
        case y: BigInt     => (x + y): BigInt

        case y: Double     => (BigDecimal(x) + y): BigDecimal
        case y: Float      => (BigDecimal(x) + y): BigDecimal
        case y: BigDecimal => (BigDecimal(x) + y): BigDecimal }
      
      // ---------------------------------------------------------------------------
      case x: BigDecimal => second match {
        case y: BigDecimal => (x + y)            : BigDecimal
        case y: BigInt     => (x + BigDecimal(y)): BigDecimal
        case y: Number     => (x + y.doubleValue): BigDecimal }
    }
  }
}

// ===========================================================================
