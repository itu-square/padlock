package padlock
package mdp

import padlock.pgcl._

/**
 * A scheduler resolves non-deterministic binary choices based on the state and
 * the program location.
 *
 * The general class able to represent the state of positionals and more complex
 * state-based schedulers (but not reach enough so far to represent history-based
 * schedulers, or probabilistic schedulers).
 */
class SimpleScheduler[Env] private (
  choices: Map[Tag, (Env, Statement, Statement) => Boolean],
  private val scope: List[Tag],
  private val seed: Long
) extends Scheduler[Env]
{
  /**
   * This is a hack to make schedulers referentially transparent (so that all
   * properties are persistent if called several times, and that objects can be
   * compared with equality).  The trick is to use the generator with a seed,
   * without ever persisiting it.  The seed itself is persistent
   */
  private lazy val genDouble = new scala.util.Random (seed).nextDouble ()
  private lazy val genLong = new scala.util.Random (seed).nextLong ()

  private type ESS2B = (Env, Statement, Statement) => Boolean

  def schedule (tag: Tag, choice: ESS2B): SimpleScheduler[Env] = {
    val choices1 = choices + (tag -> choice)
    new SimpleScheduler[Env] (choices1, scope, seed)
  }

  def const (tag: Tag, left: Boolean): SimpleScheduler[Env] = {
    val const_choice: ESS2B = { case (_,_,_) => left }
    new SimpleScheduler[Env] (choices + (tag -> const_choice), scope, seed)
  }

  def left (tag: Tag): SimpleScheduler[Env] =
    this.const (tag, true)

  def right (tag: Tag): SimpleScheduler[Env] =
    this.const (tag, false)

  def run: (SimpleScheduler[Env], SimpleScheduler[Env]) = ???

  /**
   * Resolve a demonic choice using your scheduling policy. May fail if the
   * policy is partial.
   */
  override def demonic (env: Env, stmt1: Statement, stmt2: Statement)
    : (SimpleScheduler[Env], Option[Boolean]) = {
    val resolution = for {
      tag <- scope.headOption
      choice <- choices.get (tag)
    } yield choice (env, stmt1, stmt2)
    this -> resolution
  }

  /**
   * Resolve a  probabilistic choice, using the embedded random number
   * generator.
   */
  override def probabilistic (p: Probability): (SimpleScheduler[Env], Boolean) =
    (new SimpleScheduler (choices, scope, genLong), genDouble <= p)

}

object SimpleScheduler {

  def empty[Env] (seed: Long): SimpleScheduler[Env] =
    new SimpleScheduler[Env] (
      choices = Map[Tag, (Env, Statement, Statement) => Boolean] (),
      scope = List ("_top_"),
      seed = seed)

  def apply[Env] (seed: Long): SimpleScheduler[Env] = empty (seed)

}
