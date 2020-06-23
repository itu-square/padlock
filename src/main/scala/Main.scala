object pGCL {

  type Probability = Double
  type VarSet = String

  sealed trait Value;

  sealed trait BValue extends Value
  case object True extends BValue
  case object False extends BValue

  sealed trait ValueI extends Value
  case class I (v: Int) extends ValueI

  sealed trait Expression {

    def eval (en: Env): Value = ???

  }


  case class VarExpr (v: VarSet) extends Expression
  case class ValExpr (v: Value) extends Expression
  case class BopExpr (e1: Expression, o: Operator, e2: Expression) extends Expression

  sealed trait Operator
  case object Plus extends Operator
  case object Minus extends Operator
  case object Mult extends Operator
  case object Div extends Operator
  case object Gt extends Operator
  case object Eq extends Operator
  case object Gte extends Operator

  sealed trait Statement
  case object Abort extends Statement
  case object Skip extends Statement
  case class Assgn (v: VarSet, e: Expression) extends Statement
  case class Seq (s1: Statement, s2: Statement) extends Statement
  case class PChoice (s1: Statement, p: Probability, s2: Statement) extends Statement
  case class DChoice (s1: Statement, s2: Statement) extends Statement
  case class While (g: Expression, b: Statement) extends Statement

}

object DL {

  sealed trait Formula {

    def eval (en: Env): Option[Boolean] = this match {

      case ExpressionF (ex) => 
        ex eval (en) match {
          case v : pGCL.BValue => Some (v == pGCL.True)
          case _ => None 
        }

      case NegF (f) => (f eval en) map (! _)

    }

  }

  case class ExpressionF (e: pGCL.Expression) extends Formula
  
  sealed trait BFormula extends Formula

  case class NegF (f: Formula) extends BFormula
  case class BinaryF (f1: Formula, o: Operator, f2: Formula) extends BFormula

  sealed trait Operator
  case object And extends Operator
  case object Or extends Operator
  case object Imp extends Operator
  case object Bimp extends Operator

  sealed trait QFormula extends Formula
  case class Exists (x: pGCL.VarSet, f: Formula) extends QFormula
  case class Forall (x: pGCL.VarSet, f: Formula) extends QFormula

  sealed trait MFormula extends Formula
  case class Box (s: pGCL.Statement, f: Formula) extends MFormula
  case class Update (s: Substitution, f: Formula) extends MFormula

  type Substitution = Map[pGCL.VarSet, pGCL.Expression]
}


trait Env   {

  this: Map[pGCL.VarSet, pGCL.Value] =>

  def models (f: DL.Formula): Option[Boolean] = f eval this 

}


class Main {

}
