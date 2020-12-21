package padlock
package pgcl

import scala.language.implicitConversions

package object syntax {

  implicit class NameOps (v: Name) {
    def := (e: Expression) = Assgn (v, e)

    def === (e: Expression) = VarExpr (v) === e

    def unary_! = ! VarExpr (v)
  }

  implicit def nameToVarExpr (v: Name) = VarExpr (v)
  implicit def intToValI (v: Int) = ValI (v)
  implicit def intToExpr (v: Int) = ValExpr (ValI (v))

  implicit def valueToValExpr (v: Value) = ValExpr (v)
  implicit def probabilityToValP (p: Probability) = ValP (p)

  implicit def probabilityToExpr (p: Probability) =
    ValExpr (ValP (p))

  implicit class ExpressionOps (e1: Expression) {
    def && (e2: Expression) = BExpr (e1, BinaryOperator.And, e2)
    def || (e2: Expression) = BExpr (e1, BinaryOperator.Or, e2)
    def === (e2: Expression) = BExpr (e1, BinaryOperator.Eq, e2)
    def unary_! = UExpr (UnaryOperator.Not, e1)
  }

  implicit class StatementOps (s1: Statement) {

    case class <  (p: Expression) {
      def > (s2: Statement) = Probabilistic (s1, p, s2)
    }

    def <> (s2: Statement) = Demonic (s1, s2)

    def Then (s2: Statement) = Seq (s1, s2)

    def apply (s2: Statement) = Seq (s1, s2)
  }


  implicit class IF (b: Expression) {
    case class THENOps (s1: Statement) {
      def ELSE (s2: Statement) = If (b, s1, s2)
    }
    def apply (s1: Statement) = THENOps (s1)
  }

  val SKIP = Skip

}
