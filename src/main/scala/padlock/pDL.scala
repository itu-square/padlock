package padlock

object pDL {

  import top._

  sealed trait Formula

  case class ExprF (e: pGCL.Expression) extends Formula

  sealed trait BoolF extends Formula
  case class BinaryF (
    f1: Formula,
    op: Operator,
    f2: Formula
  ) extends BoolF

  sealed trait Operator
  case object And extends Operator
  case object Or extends Operator
  case object Imp extends Operator
  case object Bimp extends Operator

  sealed trait QuantifiedF extends Formula
  case class Exists (
    v: Name,
    f: Formula
  ) extends QuantifiedF

  case class Forall (
    v: Name,
    f: Formula
  ) extends QuantifiedF

  sealed trait ModalF extends Formula

  case class Box (
    s: pGCL.Statement,
    f: Formula
  ) extends ModalF

  case class Update (
    s: Substitution,
    f: Formula
  ) extends ModalF

  type Substitution = Map[Name, pGCL.Expression]
}

