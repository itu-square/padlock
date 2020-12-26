package padlock
package mdp

class RuntimeValueSpec extends PadlockSpec {

  "0.5 is a legal value of probability" in {

    RuntimeValP (0.5).probability should
      be (Some (0.5))
  }

}
