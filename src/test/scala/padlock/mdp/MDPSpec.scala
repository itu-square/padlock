package padlock
package mdp

import cats.Applicative

import padlock.pgcl._

class MDPSpec extends PadlockSpec {

  val test0 = Probabilistic(
    Assgn("coin", ValExpr(True)),
    ValExpr(ValP(0.5)),
    Assgn("coin", ValExpr(False))
  )

  val test1 = ValExpr (ValP (0.5))

  val env0: Env = Map[String,RuntimeValue] ()

  "Regressions" - {

    "Probabilistic choice is ignored!" in {

      forAll { seed: Long =>

        val alwaysLeft =
          SimpleScheduler.AlwaysLeftScheduler[padlock.mdp.Env] (seed)

        inside (MDP.run1 (test0, alwaysLeft)) {

          case Right (env) =>
            env should not be empty

          case Left (msg) =>
            fail ("The simulator should not fail on test0!")
        }
      }
    }

    "Const expression evaluates to the inner constant!" in {

      forAll { seed: Long =>

        val alwaysLeft =
          SimpleScheduler.AlwaysLeftScheduler[padlock.mdp.Env] (seed)

        MDP.eval (test1) (env0) { value: RuntimeValue =>
            value should be (RuntimeValP (0.5))
            value.probability should be (Some  (0.5))
            implicitly[Applicative[MDP.Runner]].pure (Left ("ignore"))
        }
      }
    }
  }

}
