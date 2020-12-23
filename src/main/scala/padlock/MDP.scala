package padlock

import cats.Monad
import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import padlock.pgcl._
import padlock.pgcl.BinaryOperator._

/**
 * The trait defining the MDP evaluation for PGCL programs.  It is a trait not
 * an object (value) becuase it abstracts the scheduler. The 'Scheduler' type is
 * a type of 'dual' schedulers.  A scheduler resolves both the probabilistic
 * decisions and nondeterministic decisions.
 */

trait MDP[Scheduler[+_]] {

  /**
   * Ask the scheduler to resolve a probabilistic Boolean choice. Abstract, to
   * be provided for a particular scheduler type
   */

  def flip (p: Probability): Scheduler[Boolean]

  /**
   * Ask the scheduler to reselve a demonic Boolean choice.  Abstract, to be
   * provided for a particular scheduler type.
   */

  def demonic: Scheduler[Boolean]

  /** Scheduler[_] must be a monad, provide for the particular type */
  implicit def monadScheduler: Monad[Scheduler]



  // Concrete members

  private type SE = (Statement, Env)

  /** The type of program states: variable name - runtime value mappings */

  type Env = Map [Name, RuntimeValue]

  /** Representation of runtime values in the simulator */

  sealed trait RuntimeValue {

    def probability: Option[Probability] = None

    def boolean: Option[Boolean] = None

    def operator (operator: BinaryOperator, that: RuntimeValue)
      : Option[RuntimeValue] = None
  }

  case class RuntimeValB (v: Boolean) extends RuntimeValue {

    override def boolean: Option[Boolean] = Some (v)

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
          Some (p) filter { _ => p >= 0 && p <= 0 }

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
        case True => RuntimeValB (true)
        case False => RuntimeValB (false)
        case ValI (v) => RuntimeValI (v)
        case ValP (p) => RuntimeValP (p)
      }
  }



  /**
   * Evaluate an expression 'e' in the environment 'env'.
   *
   * Randomization should come from possible randomized environment.
   * The note is big-step on this one, but I found mixing the big-step and
   * small-step implementations for reduce/eval too difficult so I am trying
   * to do this small-step as well in CPS-style
   */

  def eval (expr: Expression) (env: Env) (k: RuntimeValue => Scheduler[Option[SE]])
    : Scheduler[Option[SE]] =
      expr match {

        case VarExpr (name) =>
          env.get (name) match {
            case None =>
              None flatTraverse k

            case Some (value) =>
              k (value)
          }

        case ValExpr (value) =>
          k (RuntimeValue (value))

        case BExpr (expr1, operator, expr2) =>
          eval (expr1) (env) { value1: RuntimeValue =>
            eval (expr2) (env) { value2: RuntimeValue =>
              value1.operator (operator, value2) match { // not entirely sure if this does not resolve to the trait method always. I think in Java it would (TODO)
                case None => None flatTraverse k
                case Some (v) => k (v)
              }
            }
          }

        // TODO: An interesting warning that needs to be studied
        case UExpr (UnaryOperator.Not, expr) =>
          eval (expr) (env) { value: RuntimeValue =>
            value.boolean match {
              case None => None flatTraverse k
              case Some (b) => k (RuntimeValB (!b))
            }
          }
      }


  /**
   * Reduce statement 'stmt' in environment 'env'  and feed the result into the
   * continuation function 'k'
   */

  def reduce (stmt: Statement) (env: Env) (k: SE => Scheduler[Option[SE]])
    : Scheduler[Option[SE]] = {
    stmt match {

      case Skip =>
        k (Skip -> env)

      case Abort =>
        None flatTraverse k

      case Assgn (variable, expr) =>
        eval (expr) (env) { value: RuntimeValue =>
          val env1 = env + (variable -> value)
          k (Skip -> env1)
        }

      case Seq (stmt1, stmt2) =>
        reduce (stmt1) (env)  {
          case (stmt11, env1) =>
            val stmt = if (stmt11 != Skip) Seq (stmt11, stmt2) else stmt2
            k (stmt -> env1)
        }


      case Probabilistic (stmt1, pr, stmt2) =>
        eval (pr) (env) { value: RuntimeValue =>
          value.probability match {

            case None =>
              k (Abort -> env)

            case Some (probability) =>
              for {
                left <- flip (probability)
                stmt = if (left) stmt1 else stmt2
                ose <- k (stmt -> env) // is this tail recursive?
              } yield ose
          }
        }


      case Demonic (stmt1, stmt2) =>
        for {
          left <- this.demonic
          stmt = if (left) stmt1 else stmt2
          ose <- k (stmt -> env) // is this tail recursive?
        } yield ose


      case If (condition, stmt1, stmt2) =>
        eval (condition) (env) { value: RuntimeValue =>
          value.boolean match {

            case None =>
              k (Abort -> env)

            case Some (choice) =>
              val stmt = if (choice) stmt1 else stmt2
              k (stmt -> env)
          }
        }


      case While (guard,body) =>
        eval (guard) (env) { value: RuntimeValue =>
          value.boolean match {

            case None =>
              k (Abort -> env)

            case Some (continue) =>
              if (continue)
                k (Seq (body, stmt) -> env)
              else
                k (Skip -> env)
          }
        }

    }
  }

  /** Execute one run of the system (impure). */
  def run1 (s: Statement) (env: Env = Map ()): Env =  {
    // reduce (s) (env) (se => monadScheduler.pure (Some (se)))
    // .run (with some newly initialized seed
    // .valueA (get hte value out of it
    ???
  }
}
