package padlock

object pGCL {

  import top._

  sealed trait Value;

  sealed trait ValB extends Value
  case object True extends ValB
  case object False extends ValB

  case class ValI (v: Int) extends Value
  case class ValP (p: Probability) extends Value


  sealed trait Expression {

    /** A regular evaluator for expressions, possibly nondeterministic */
    def eval (en: Env): Value =
      ???

  }

  case class VarExpr (v: Name) extends Expression
  case class ValExpr (v: Value) extends Expression

  case class BExpr (
    e1: Expression,
    op: BinaryOperator,
    e2: Expression
  ) extends Expression

  sealed trait BinaryOperator
  case object Plus extends BinaryOperator
  case object Minus extends BinaryOperator
  case object Mult extends BinaryOperator
  case object Div extends BinaryOperator
  case object Gt extends BinaryOperator
  case object Eq extends BinaryOperator
  case object Gte extends BinaryOperator

  object UnaryOperator extends Enumeration {
    type UnaryOperator = Value
    val Not = Value
  }
  import UnaryOperator._

  case class UExpr (
    op: UnaryOperator,
    e: Expression
  ) extends Expression


  sealed trait Statement
  case object Abort extends Statement
  case object Skip extends Statement

  case class Assgn (
    v: Name,
    e: Expression
  ) extends Statement

  case class Seq (
    s1: Statement,
    s2: Statement
  ) extends Statement

  case class Probabilistic (
    s1: Statement,
    pr: Expression,
    s2: Statement
  ) extends Statement

  case class Demonic (
    s1: Statement,
    s2: Statement
  ) extends Statement

  case class If (
    hd: Expression,
    thn: Statement,
    els: Statement
  ) extends Statement

  case class While (
    guard: Expression,
    body: Statement
  ) extends Statement

}
