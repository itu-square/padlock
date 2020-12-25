package padlock

package object mdp {

  /** The type of program states: variable name - runtime value mappings */
  type Env = Map [Name, RuntimeValue]

}
