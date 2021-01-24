package padlock.mdp

import cats.Eval
import cats.Applicative
import cats.data.State
import cats.instances.option._
import cats.instances.either._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._

import padlock._
import padlock.pgcl._
import padlock.pgcl.BinaryOperator._

/**
 * The MDP semantics (evaluation) for PGCL programs. The 'Runner' type is a type
 * of 'dual' schedulers.  They are dual in the sense that they resolve both the
 * probabilistic decisions and nondeterministic decisions.
 */

object MDP {

  /** A convenience type name */
  private type SE = (Statement, Env)


  /** The type representing the internal state of the executor */
  type Runner[A] = State[Scheduler[Env, Statement], A]


  def runner[A] (f: Scheduler[Env, Statement] => (Scheduler[Env, Statement], A))
  : State[Scheduler[Env, Statement], A] =
      State[Scheduler[Env, Statement], A] (f)


  /**
   * Ask the scheduler to resolve a probabilistic Statement choice. Abstract, to
   * be provided for a particular scheduler type
   */
  def probabilistic (p: Probability, left: Statement, right: Statement)
  : Runner[Statement] =
    State { s: Scheduler[Env, Statement] => s.probabilistic (p, left, right) }


  /**
   * Ask the scheduler to reselve a demonic Boolean choice.  Delegates to the
   * scheduler. Returns None if the Scheduler's policy failed.  In such case, we
   * should Abort like with any other error.
   */
  // TODO: Einar suggested to change this to Runner[Option[Statement]].
  // TODO: We could assume that the policy is complete and get rid of the option
  // for simplicity. We already have Abort for that.

  def demonic (env: Env, stmt1: Statement, stmt2: Statement)
    : Runner[Option[Statement]] =
    State { s: Scheduler[Env, Statement] =>
      s.demonic (env, stmt1, stmt2) }

  /**
   * Evaluate an expression 'e' in the environment 'env'.
   *
   * Randomization should come from possible randomized environment.
   * The note is big-step on this one, but I found mixing the big-step and
   * small-step implementations for reduce/eval too difficult so I am trying
   * to do this small-step as well in CPS-style
   */

  def eval (expr: Expression) (env: Env)
    (k: RuntimeValue => Runner[Either[String, SE]])
      : Runner[Either[String, SE]] =
        expr match {

          case VarExpr (name) =>
            env.get (name) match {
              case None =>
                implicitly[Applicative[Runner]]
                  .pure (Left (s"Undefined variable $name!"))

              case Some (value) =>
                k (value)
            }

          case ValExpr (value) =>
            k (RuntimeValue (value))

          case BExpr (expr1, operator, expr2) =>
            eval (expr1) (env) { value1: RuntimeValue =>
              eval (expr2) (env) { value2: RuntimeValue =>
                value1.operator (operator, value2) match {
                  // not entirely sure if this does not resolve to the trait method always. I think in Java it would (TODO)

                  case None =>
                    implicitly[Applicative[Runner]]
                      .pure (Left (s"Incorrect operator in a Boolean expression!"))

                  case Some (value) =>
                    k (value)
                }
              }
            }

          // TODO: An interesting warning that needs to be studied
          case UExpr (UnaryOperator.Not, expr) =>
            eval (expr) (env) { value: RuntimeValue =>
              value.boolean match {

                case None =>
                  implicitly[Applicative[Runner]]
                    .pure (Left (s"Incorrect operator in a Boolean expression!"))

                case Some (b) =>
                  k (RuntimeValB (!b))
              }
            }
        }


  /**
   * Reduce statement 'stmt' in environment 'env'  and feed the result into the
   * continuation function 'k'
   */

  def reduce (stmt: Statement) (env: Env) (k: SE => Runner[Either[String, SE]])
    : Runner[Either[String, SE]] =
    stmt match {

      case Skip =>
        k (Skip -> env)

      case Abort (msg) =>
        implicitly[Applicative[Runner]].pure (Left (msg))

      case Assgn (variable, expr) =>
        eval (expr) (env) { value: RuntimeValue =>
          val env1 = env + (variable -> value)
          k (Skip -> env1)
        }

      case Seq (stmt1, stmt2) =>
        reduce (stmt1) (env)  {
          case (stmt11, env1) =>
            val stmt = if (stmt11 != Skip) Seq (stmt11, stmt2) else stmt2
            reduce (stmt) (env1) (k)
            // TODO (test the following, which I would find a more natural rule)
            // reduce (stmt1) (env1) (se => reduce (stmt2) (se._2) (k))
        }


      case Probabilistic (stmt1, pr, stmt2) =>
        eval (pr) (env) { value: RuntimeValue =>
          value.probability match {

            case None =>
              k (Abort ("Illegal value of probability!") -> env)

            case Some (probability) =>
              for {
                stmt <- probabilistic (probability, stmt1, stmt2)
                ese <- reduce (stmt) (env) (k) // is this tail recursive?
              } yield ese
          }
        }


      case Demonic (stmt1, stmt2) =>
        for {
          ostmt <- this.demonic (env, stmt1, stmt2)
          stmt = ostmt getOrElse (Abort ("Incomplete scheduler policy!"))
          ese <- reduce (stmt) (env) (k) // is this tail recursive?
        } yield ese


      case If (condition, stmt1, stmt2) =>
        eval (condition) (env) { value: RuntimeValue =>
          value.boolean match {

            case None =>
              k (Abort ("Non-Boolean condition in 'if'!") -> env)

            case Some (choice) =>
              val stmt = if (choice) stmt1 else stmt2
              reduce (stmt) (env) (k)
          }
        }


      case While (guard,body) =>
        eval (guard) (env) { value: RuntimeValue =>
          value.boolean match {

            case None =>
              k (Abort ("Non-Boolean condition in 'while'!") -> env)

            case Some (continue) =>
              if (continue)
                reduce (Seq (body, stmt)) (env) (k)
              else
                k (Skip -> env)
          }
        }

      case Scope (tag, body) =>
        for {
          _ <- State.modify[Scheduler[Env, Statement]] { _.enter (tag) }
          ose <- reduce (body) (env: Env) { se =>
                  runner[Either[String, SE]] { s2: Scheduler[Env, Statement] =>
                    s2.leave match {
                      case None =>
                        k (
                          Abort ("Leaving top scope is impossible!") -> se._2
                        ).run (s2).value
                      case Some (s3) =>
                        k (se).run (s3).value
                    }
                  }
                }
        } yield ose
    }


  /** Execute one run of the system (impure). */
  def run1
    (stmt: Statement,
     scheduler: Scheduler[Env, Statement] =
       SimpleScheduler.FairCoinScheduler (42),
     env: Env = Map ())
    : Either[String, Env] =  {
      reduce (stmt) (env) (se =>
        implicitly[Applicative[Runner]].pure (Right (se)))
        .run (scheduler)
        .value
        ._2
        .map { _._2 }
    }
}
