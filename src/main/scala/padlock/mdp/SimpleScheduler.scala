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
class SimpleScheduler[Env, Out] private (
  policy: Map[Tag, Env => Boolean],
  private val scope: List[Tag],
  private val seed: Long
) extends Scheduler[Env, Out]
{
  /**
   * This is a hack to make schedulers referentially transparent (so that all
   * properties are persistent if called several times, and that objects can be
   * compared with equality).  The trick is to use the generator with a seed,
   * without ever persisiting it.  The seed itself is persistent
   */
  private lazy val genDouble =
    new scala.util.Random (seed).nextDouble ()

  private lazy val genLong =
    new scala.util.Random (seed).nextLong ()


  def schedule (tag: Tag) (choice: Env => Boolean)
    : SimpleScheduler[Env, Out] = {
      val policy1 = policy + (tag -> choice)
      new SimpleScheduler[Env, Out] (policy1, scope, seed)
    }


  def const (tag: Tag, left: Boolean): SimpleScheduler[Env, Out] = {
    def const_choice (e: Env): Boolean = left
    new SimpleScheduler[Env, Out] (policy + (tag -> const_choice), scope, seed)
  }


  def left (tag: Tag): SimpleScheduler[Env, Out] =
    this.const (tag, true)


  def right (tag: Tag): SimpleScheduler[Env, Out] =
    this.const (tag, false)


  /**
   * Resolve a demonic choice using your scheduling policy. May fail if the
   * policy is partial.
   */
  override def demonic (env: Env): (SimpleScheduler[Env, Out], Option[Boolean]) = {

    val resolution = for {
      tag <- scope.find { policy isDefinedAt _ }
      choice <- policy.get (tag)
    } yield choice (env)
    this -> resolution
  }

  /**
   * Resolve a  probabilistic choice, using the embedded random number
   * generator.
   */
  override def probabilistic (p: Probability)
  : (SimpleScheduler[Env, Out], Boolean) =
      (new SimpleScheduler (policy, scope, genLong), genDouble <= p)


  override def enter (scope: Tag): Scheduler[Env, Out] =
    new SimpleScheduler[Env, Out] (
      policy = this.policy,
      scope = scope:: this.scope,
      seed = this.seed
    )

  override def leave: Option[Scheduler[Env, Out]] =
    for {
      _ <- scope.headOption
      scope1 = this.scope.tail
    } yield new SimpleScheduler[Env, Out] (this.policy, scope1, seed)

}

object SimpleScheduler {

  private val _top_ : Tag = "_top_"

  def empty[Env, Out] (seed: Long): SimpleScheduler[Env, Out] =
    new SimpleScheduler[Env, Out] (
      policy = Map[Tag, Env => Boolean] (),
      scope = List (_top_),
      seed = seed
    )


  /** A convenience constructor for building empty schedulers */
  def apply[Env, Out] (seed: Long): SimpleScheduler[Env, Out] =
    empty (seed)


  /** A bunch of predefined schedulers */

  def AlwaysLeftScheduler[Env, Out] (seed: Long): SimpleScheduler[Env, Out] =
    this.empty[Env, Out] (seed)
      .left (_top_)

  def AlwaysRightScheduler[Env, Out] (seed: Long): SimpleScheduler[Env, Out] =
    this.empty[Env, Out] (seed)
      .right (_top_)

  /**
   * A scheduler that tosses a fair coin at each binary demonic choice.
   * This is (obviously) not a positional scheduler.
   *
   * Not sure that this is a simple scheduler, but I hacked it in.
   */
  def FairCoinScheduler[Env, Out] (seed: Long)
  : SimpleScheduler[Env, Out] =
      new SimpleScheduler[Env, Out] (
        policy = Map[Tag, Env => Boolean] (),
        scope = List (_top_),
        seed = seed) {

        override def demonic (env: Env)
          : (SimpleScheduler[Env, Out], Option[Boolean]) = {
          val (scheduler, choice) =  probabilistic (0.5)
          scheduler -> Some (choice)
        }
      }

}
