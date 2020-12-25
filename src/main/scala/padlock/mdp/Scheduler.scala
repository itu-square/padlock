package padlock
package mdp

import padlock.pgcl._

/**
 * A scheduler resolves non-deterministic binary choices based on the state and
 * the program location. This is the interface that shall be implemented by all
 * schedulers.
 */
trait Scheduler[Env] {

  /**
   * Resolve a demonic choice in state 'env' between statements 'stmt1' and
   * 'stmt2'.   It can produce a new value of a scheduler since in general
   * schedulers can have memory, so each resolution can be remembered.  In
   * memory-less schedulers the returned object should be the same as 'this'.
   *
   * @return a new scheduler and a Boolean stating if the left (stmt1) choice
   *         should be taken.  None, if the policy was incomplete and failed.
   */
  def demonic (env: Env, stmt1: Statement, stmt2: Statement)
    : (Scheduler[Env], Option[Boolean])

  /**
   * Resolve a  probabilistic choice, using the embedded random number
   * generator.  The new scheduler embeds a random generator with a new seed.
   *
   * @return a new scheduler and a Boolean stating if the left choice
   *         should be taken.
   */
  def probabilistic (p: Probability): (Scheduler[Env], Boolean)


  def enter (scope: Tag): Scheduler[Env]

  /** Returns None if attempting to leave the top scope */
  def leave: Option[Scheduler[Env]]

}

