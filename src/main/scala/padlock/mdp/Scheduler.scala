package padlock
package mdp

import padlock.pgcl._

/**
 * A scheduler resolves non-deterministic binary choices based on the state and
 * the program location. This is the interface that shall be implemented by all
 * schedulers.
 *
 * Env - type of the state values (may be used to influence schedulers), this
 *       could also include program point or the AST evaluated, or an
 *       alternative representation of  policy
 * Out - type of  the values selected by the scheduler
 */
trait Scheduler[Env, Out] {

  /**
   * Resolve a demonic choice in state 'env' between statements 'stmt1' and
   * 'stmt2'.   It can produce a new value of a scheduler since in general
   * schedulers can have memory, so each resolution can be remembered.  In
   * memory-less schedulers the returned object should be the same as 'this'.
   *
   * @return a new scheduler and a Boolean stating if the left (stmt1) choice
   *         should be taken.  None, if the policy was incomplete and failed.
   */
  def demonic (env: Env, left: Out, right: Out)
    : (Scheduler[Env, Out], Option[Out]) = {
    val (scheduler, resolution) = this.demonic (env)
    val choice = resolution map { if (_) left else right }
    scheduler -> choice
  }

  def demonic (env: Env): (Scheduler[Env, Out], Option[Boolean])

  /**
   * Resolve a  probabilistic choice, using the embedded random number
   * generator.  The new scheduler embeds a random generator with a new seed.
   *
   * @return a new scheduler and a Boolean stating if the left choice
   *         should be taken.
   */
  def probabilistic (p: Probability, left: Out, right: Out)
  : (Scheduler[Env, Out], Out) = {
    val (scheduler, resolution) = this.probabilistic (p)
    val choice = if (resolution) left else right
    scheduler -> choice
  }

  def probabilistic (p: Probability): (Scheduler[Env,Out], Boolean)

  /**
   * Enter a named scope.  Scope names are used to construct
   * scheduler policies.
   */
  def enter (scope: Tag): Scheduler[Env,Out]

  /**
   * Leave a named scope.  Returns None if attempting to leave the top scope
   */
  def leave: Option[Scheduler[Env,Out]]

}

