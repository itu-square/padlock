package padlock

package object pgcl {

  sealed trait Value;

  sealed trait ValB extends Value
  case object True extends ValB
  case object False extends ValB

  case class ValI (v: Int) extends Value
  case class ValP (p: Probability) extends Value

  sealed trait Expression
  case class VarExpr (v: Name) extends Expression
  case class ValExpr (v: Value) extends Expression

  object BinaryOperator extends Enumeration {
    type BinaryOperator = Value
    val Plus, Minus, Mult, Div, Gt, Eq, Gte, And, Or = Value
  }
  import BinaryOperator._

  case class BExpr (
    expr1: Expression,
    operator: BinaryOperator,
    expr2: Expression
  ) extends Expression

  object UnaryOperator extends Enumeration {
    type UnaryOperator = Value
    val Not = Value
  }
  import UnaryOperator._

  case class UExpr (
    operator: UnaryOperator,
    expr: Expression
  ) extends Expression

  sealed trait Statement
  case class Abort (msg: String = "Aborted") extends Statement
  case object Skip extends Statement

  case class Assgn (
    variable: Name,
    expression: Expression
  ) extends Statement

  case class Seq (
    stmt1: Statement,
    stmt2: Statement
  ) extends Statement

  case class Probabilistic (
    stmt1: Statement,
    probability: Expression,
    stmt2: Statement
  ) extends Statement

  case class Demonic (
    stmt1: Statement,
    stmt2: Statement
  ) extends Statement

  case class If (
    condition: Expression,
    stmt1: Statement,
    stmt2: Statement
  ) extends Statement

  case class While (
    guard: Expression,
    body: Statement
  ) extends Statement

  type Tag = String

  case class Scope (
    tag: Tag,
    body: Statement
  ) extends Statement


}
