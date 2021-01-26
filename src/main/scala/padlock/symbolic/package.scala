package padlock
package symbolic

import cats.data.State

trait SymbolicVar {

  // supports equality test? unification?
  // it probably has some concept of name

}


trait Executor[SymbolicExpr, PathCondition] {


    // The type of symbolic expression is whatever the  type of the variable is.
    type SymbolicEnv = Map [Name,SymbolicExpr]

    // Is an expectation mapping of states to real values? There is something
    // inadequate, in this representation, as in general it becomes infinite I
    // think.  Do we have an idea of representation here? Closure for the
    // representation?
    //
    // The type of the SymbolicExpr should be a real number (we might need to
    // add a type checker to the tool)

    type SymbolicExpectation = Map[SymbolicEnv,SymbolicExpr]

    // This expressions should be of Boolean type (or perhaps even more), let's
    // see what shows up
    type PathCondition = SymbolicExpr

    type WorkList = LazyList[(SymbolicExpectation, pdl.Formula)]

    /** First attempt: expand one formula. We fixpoint later */
    def reduce (formula: pdl.Formula)
      (expectation: SymbolicExpectation, path: PathCondition)
      : WorkList =

        formula match {

          case pdl.BinaryF (f1, pdl.And, f2) =>
            // val pairs = reduce (f1, senv)
            // could zip all of thm with f2 into and and return as an empty list.
            ???

          case pdl.BinaryF (f1, pdl.Or, f2) =>
            ???

          case pdl.Exists (variable, formula) =>
            ???

          case pdl.Forall (variable, formula) =>
            ???

          case pdl.Box (stmt, formula) =>
            reduceBox (stmt, formula) (expectation, path)

          case pdl.Update (subst, formula) =>
            ???

          case _ =>
            ???
        }


    def reduceBox (stmt: pgcl.Statement, formula: pdl.Formula)
      (expectation: SymbolicExpectation, path: PathCondition): WorkList =
      stmt match {

        case pgcl.Skip => ???
          // inline (formula, subst)
          ???
          // Q. How do we know that the formula we obtained is a first order
          // formula with no box? Cannot there be any other box nested later?
          //
          // Q. Now if we have many in the expectation, in which one to inline?
          // all of them? Or perhaps we should turn it into a pair? (from a map)

        case _ =>
          ???


      }


    def inline (formula: pdl.Formula, subst: SymbolicEnv) = ???


    // What do I need to specify?
    //
    // That after executing this program the expected value of a certain
    // expression Y (state) will be X?
    // Ideally this should be at least multivariate.
    // So we start with a mapping of environments to symbolic variables
    //



//  SEnv = SymEnv X PathCondition
//  // expand all branches.
//  // one choice: the environment is Gamma, U, Phi
//  def reducePDL (stmt: Statement, env: SEnv): Set[(Statement, SEnv)]
//  // We unfold up to K times
//  // We tactics/strategies and rules should be separated
//
//    def K: Int
//
//    require (K >= 1)


}
