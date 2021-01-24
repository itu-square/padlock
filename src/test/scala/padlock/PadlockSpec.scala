package padlock

trait PadlockSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.Checkers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.Inside
