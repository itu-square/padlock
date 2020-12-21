package padlock

import cats.Monad

trait Evaluator[Language, Result[_]] {

  implicit def mr: Monad[Result]

  def eval: AnyVal

}

trait MDP[Language, Result[_], Randomness, Scheduler]
  extends Evaluator[Language, Result] {

  implicit def mr: Monad[Result]
  def eval: AnyVal

}

trait SymbolicMDP[Language, Result, Randomness, Scheduler]
