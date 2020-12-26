package padlock
package mdp

import pgcl.BinaryOperator._

/**
 * Representation of runtime values in the simulator.
 **/

sealed trait RuntimeValue {

  def probability: Option[Probability] = None

  def boolean: Option[Boolean] = None

  def operator (operator: BinaryOperator, that: RuntimeValue)
    : Option[RuntimeValue] = None
}



case class RuntimeValB (v: Boolean) extends RuntimeValue {

  override def boolean: Option[Boolean] = Some (v)

  /**
   * TODO: I suspect this does not resolve with the other methods
   * polymorphically.  Write a regression test and fix.
   */
  def operator (operator: BinaryOperator, that: RuntimeValB)
    : Option[RuntimeValue] =
    operator match {
      case And => Some (RuntimeValB (this.v && that.v))
      case Or  => Some (RuntimeValB (this.v && that.v))
      case Eq  => Some (RuntimeValB (this.v == that.v))
      case Gt  => Some (RuntimeValB (this.v >  that.v))
      case Gte => Some (RuntimeValB (this.v >= that.v))
      case _   => None
    }

}

case class RuntimeValI (v: Int) extends RuntimeValue {

  def operator (operator: BinaryOperator, that: RuntimeValI)
    : Option[RuntimeValue] =
    operator match {
      case Plus  => Some (RuntimeValI (this.v  + that.v))
      case Minus => Some (RuntimeValI (this.v  - that.v))
      case Mult  => Some (RuntimeValI (this.v  * that.v))
      case Div   => Some (RuntimeValI (this.v  / that.v))
      case Gt    => Some (RuntimeValB (this.v  > that.v))
      case Eq    => Some (RuntimeValB (this.v == that.v))
      case Gte   => Some (RuntimeValB (this.v >= that.v))
      case _     => None
    }

}

case class RuntimeValP (p: Probability) extends RuntimeValue {

  override def probability: Option[Probability] =
        Some (p) filter { _ => p >= 0 && p <= 1 }

  def operator (operator: BinaryOperator, that: RuntimeValP)
    : Option[RuntimeValue] = {

    def safe (p: Probability): Option[RuntimeValP] =
      Some (RuntimeValP (p)) filter { _ => p >= 0 && p <= 0 }

    operator match {
      case Plus  => safe (this.p  + that.p)
      case Minus => Some (RuntimeValP (this.p  - that.p))
      case Mult  => Some (RuntimeValP (this.p  * that.p))
      case Div   => Some (RuntimeValP (this.p  / that.p))
      case Gt    => Some (RuntimeValB (this.p  > that.p))
      case Eq    => Some (RuntimeValB (this.p == that.p))
      case Gte   => Some (RuntimeValB (this.p >= that.p))
      case _     => None
    }

  }
}

object RuntimeValue {

  /**
   * A convenience constructor converting PGCL syntax literals (known as
   * Value) to RuntimeValue.
   */
  def apply (v: pgcl.Value): RuntimeValue =
    v match {
      case pgcl.True => RuntimeValB (true)
      case pgcl.False => RuntimeValB (false)
      case pgcl.ValI (v) => RuntimeValI (v)
      case pgcl.ValP (p) => RuntimeValP (p)
    }
}
