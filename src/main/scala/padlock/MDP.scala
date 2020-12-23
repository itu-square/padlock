package padlock

import cats.Monad
import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import padlock.pgcl._

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
  private type SO[SE] = Scheduler[Option[SE]]

  sealed trait RuntimeValue {

    def probability: Option[Probability] = None

    def boolean: Option[Boolean] = None
  }

  case class RuntimeValB (v: Boolean) extends RuntimeValue {
    override def boolean: Option[Boolean] = Some (v)
  }

  case class RuntimeValI (v: Int) extends RuntimeValue

  case class RuntimeValP (p: Probability) extends RuntimeValue {
    override def probability: Option[Probability] =
          Some (p) filter { _ => p >= 0 && p <= 0 }
  }

  type Env = Map [Name, RuntimeValue]

  /**
   * Evaluate an expression 'e' in the environment 'env'.
   *
   * Randomization should come from possible randomized environment.
   * The note is big-step on this one, but I found mixing the big-step and
   * small-step implementations for reduce/eval too difficult so I am trying
   * to do this small-step as well in CPS-style
   */

  def eval (e: Expression) (env: Env) (k: RuntimeValue => SO[SE]) = ???




  /**
   * Reduce statement 'stmt' in environment 'env'  and feed the result into the
   * continuation function 'k'
   */

  def reduce (stmt: Statement) (env: Env) (k: SE => Scheduler[Option[SE]]): Scheduler[Option[SE]] = {
    stmt match {

      case Skip =>
        k (Skip -> env)

      case Abort =>
        None.flatTraverse (k)

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
