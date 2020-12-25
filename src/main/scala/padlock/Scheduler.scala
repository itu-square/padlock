package padlock

import padlock.pgcl._

/**
 * A scheduler resolves non-deterministic binary choices based on the state and
 * the program location.
 *
 * The general class able to represent the state of positionals and more complex
 * state-based schedulers (but not reach enough so far to represent history-based
 * schedulers, or probabilistic schedulers).
 */
class Scheduler[Env] (
  choices: Map [Tag, (Env, Statement, Statement) => Boolean])
{
  private type ESS2B = (Env, Statement, Statement) => Boolean

  def schedule (tag: Tag, choice: ESS2B): Scheduler[Env] = {
    val choices1 = choices + (tag -> choice)
    new Scheduler[Env] (choices1)
  }

  def const (tag: Tag, left: Boolean): Scheduler[Env] = {
    val const_choice: ESS2B = { case (_,_,_) => left }
    new Scheduler[Env] (choices + (tag -> const_choice))
  }

  def left (tag: Tag): Scheduler[Env] =
    this.const (tag, true)

  def right (tag: Tag): Scheduler[Env] =
    this.const (tag, false)

}

object Scheduler {

  def empty[Env]: Scheduler[Env] =
    new Scheduler[Env] (Map[Tag, (Env, Statement, Statement) => Boolean] ())

}
