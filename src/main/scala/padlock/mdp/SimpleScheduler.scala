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
  policy: Map[Tag, (Env, Statement, Statement) => Boolean],
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

  def schedule (tag: Tag) (choice: (Env, Statement, Statement) => Boolean)
    : SimpleScheduler[Env] = {
      val policy1 = policy + (tag -> choice)
      new SimpleScheduler[Env] (policy1, scope, seed)
    }

  def const (tag: Tag, left: Boolean): SimpleScheduler[Env] = {
    def const_choice (e: Env, s1: Statement, s2: Statement):  Boolean = left
    new SimpleScheduler[Env] (policy + (tag -> const_choice), scope, seed)
  }

  def left (tag: Tag): SimpleScheduler[Env] =
    this.const (tag, true)

  def right (tag: Tag): SimpleScheduler[Env] =
    this.const (tag, false)


  /**
   * Resolve a demonic choice using your scheduling policy. May fail if the
   * policy is partial.
   */
  override def demonic (env: Env, stmt1: Statement, stmt2: Statement)
    : (SimpleScheduler[Env], Option[Boolean]) = {
    val resolution = for {
      tag <- scope.find { policy isDefinedAt _ }
      choice <- policy.get (tag)
    } yield choice (env, stmt1, stmt2)
    this -> resolution
  }

  /**
   * Resolve a  probabilistic choice, using the embedded random number
   * generator.
   */
  override def probabilistic (p: Probability): (SimpleScheduler[Env], Boolean) =
    (new SimpleScheduler (policy, scope, genLong), genDouble <= p)


  override def enter (scope: Tag): Scheduler[Env] =
    new SimpleScheduler[Env] (
      policy = this.policy,
      scope = scope:: this.scope,
      seed = this.seed
    )

  override def leave: Option[Scheduler[Env]] =
    for {
      _ <- scope.headOption
      scope1 = this.scope.tail
    } yield new SimpleScheduler[Env] (this.policy, scope1, seed)

}

object SimpleScheduler {

  private val _top_ : Tag = "_top_"

  def empty[Env] (seed: Long): SimpleScheduler[Env] =
    new SimpleScheduler[Env] (
      policy = Map[Tag, (Env, Statement, Statement) => Boolean] (),
      scope = List (_top_),
      seed = seed)

  /** A convenience constructor for building empty schedulers */
  def apply[Env] (seed: Long): SimpleScheduler[Env] = empty (seed)

  /** A bunch of predefined schedulers */

  def AlwaysLeftScheduler[Env] (seed: Long): SimpleScheduler[Env] =
    this.empty[Env] (seed)
      .left (_top_)

  def AlwaysRightScheduler[Env] (seed: Long): SimpleScheduler[Env] =
    this.empty[Env] (seed)
      .right (_top_)

  /**
   * A scheduler that tosses a fair coin at each binary demonic choice.
   * This is (obviously) not a positional scheduler.
   *
   * Not sure that this is a simple scheduler, but I hacked it in.
   */
  def FairCoinScheduler[Env] (seed: Long): SimpleScheduler[Env] =
    new SimpleScheduler[Env] (
      policy = Map[Tag, (Env, Statement, Statement) => Boolean] (),
      scope = List (_top_),
      seed = seed) {

      override def demonic (env: Env, stmt1: Statement, stmt2: Statement)
        : (SimpleScheduler[Env], Option[Boolean]) =
          probabilistic (0.5) match {
            case (scheduler, choice) => scheduler -> Some (choice)
          }
    }

}
