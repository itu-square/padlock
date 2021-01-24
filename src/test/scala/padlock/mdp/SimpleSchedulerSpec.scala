package padlock
package mdp

import org.scalacheck.Prop

class SimpleSchedulerSpec extends PadlockSpec {

// TODO: show that there exists some seeds for which each scheduler
// resolves the probabilistic choice to true (respectively to false)
//
// Hopefully We can use Prop.exists (from scalacheck/scalatest)
//
// Manual testing was suspicious so far.

  "Probabilistic choice"  - {

    "can resolve to false" in check {
      Prop.exists  { seed: Long =>
        !SimpleScheduler
          .empty[Unit] (seed)
          .probabilistic (0.5)
          ._2
      }
    }

    "can resolve to true" in check {
      Prop.exists  { seed: Long =>
        SimpleScheduler
          .empty[Unit] (seed)
          .probabilistic (0.5)
          ._2
      }
    }

    "is referentially transparent (1)" in check {
      Prop.forAll  { seed: Long =>

        val v1 = SimpleScheduler
                   .empty[Unit] (seed)
                   .probabilistic (0.5)
                   ._2

        val v2 = SimpleScheduler
                   .empty[Unit] (seed)
                   .probabilistic (0.5)
                   ._2

        v1 == v2
      }
    }

    "is referentially transparent (2)" in check {
      Prop.forAll  { seed: Long =>
        val s = SimpleScheduler.empty[Unit] (seed)
        s.probabilistic (0.5)._2 == s.probabilistic (0.5)._2
      }
    }

  }

  "Demonic choice (FairCoinScheduler)" - {

    "can resolve to false" in check {
      Prop.exists  { seed: Long =>
        !SimpleScheduler
          .FairCoinScheduler[Unit] (seed)
          .demonic ((), pgcl.Skip, pgcl.Skip)
          ._2
          .getOrElse (true)
      }
    }

    "can resolve to true" in check {
      Prop.exists  { seed: Long =>
        SimpleScheduler
          .FairCoinScheduler[Unit] (seed)
          .demonic ((), pgcl.Skip, pgcl.Skip)
          ._2
          .getOrElse (false)
      }
    }

  }

}

