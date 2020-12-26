package padlock
package mdp

import org.scalacheck._

import padlock.pgcl._


class MDPSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatest.Inside {

  val alwaysLeft =
    SimpleScheduler.AlwaysLeftScheduler[padlock.mdp.Env] (42)

  val test0 = Probabilistic(
    Assgn("coin", ValExpr(True)),
    ValExpr(ValP(0.5)),
    Assgn("coin", ValExpr(False))
  )

  "Regressions of the reducer" - {

    "Probabilistic choice is ignored!" in {

      inside (MDP.run1 (test0, alwaysLeft)) {
        case Right (env) =>
          env should not be empty
        case Left (msg) =>
          fail ("The simulator should not fail on test0!")
      }

    }

  }
}
