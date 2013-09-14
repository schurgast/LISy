/*
 * @author Stefan Schurgast
 */

package ch.uzh.ifi.ddis.markovlogicinference

object a extends DefaultVariable("a")
object b extends DefaultVariable("b")
object c extends DefaultVariable("c")
object d extends DefaultVariable("d")
object e extends DefaultVariable("e")
object f extends DefaultVariable("f")
object g extends DefaultVariable("g")
object h extends DefaultVariable("h")
object i extends DefaultVariable("i")
object j extends DefaultVariable("j")
object k extends DefaultVariable("k")
object l extends DefaultVariable("l")
object m extends DefaultVariable("m")
object n extends DefaultVariable("n")
object o extends DefaultVariable("o")
object p extends DefaultVariable("p")
object q extends DefaultVariable("q")
object r extends DefaultVariable("r")
object s extends DefaultVariable("s")
object t extends DefaultVariable("t")
object u extends DefaultVariable("u")
object v extends DefaultVariable("v")
object w extends DefaultVariable("w")
object x extends DefaultVariable("x")
object y extends DefaultVariable("y")
object z extends DefaultVariable("z")

/**
* Abstract class Variable represents variables in Markov Logic Formulas.
*/
abstract class Variable

/**
* DefaultVariables extend Variables with special Markov Logic syntax. The
* symbols herefore are *, ! and +.
*
* if predicates in formulas are preceeded by *, the formula can either be true
* or false. E.g. * student(x) ^ professor(x) means either student(x) ^ professor(x)
* or !student(x) ^ professor(x)
*
* if variables are preceded by +, a separate weight is learned for each formula
* obtained by grounding that variable to one of its values. E.g. hasPosition(x,+y)
* there is a seperate weight to be learned for each value of y.
*
* if a ! follows a variable, the variable has mutually exclusive and exhaustive
* values. e.g. hasPosition(x,y!) means that every person x has exactly one position y.
*/
case class DefaultVariable(name: String) extends Variable {
  def ! = new ExaclyOneOfDomain(this)
  def + = new OneWeightPerBinding(this)
  //def * = new ...(this)
  override def toString = name
}

// TODO: not implemented yet in reasoning algorithm
class ExaclyOneOfDomain(variable: DefaultVariable) extends Variable {
  override def toString = variable.toString + "!"
}

// TODO: not implemented yet in reasoning algorithm
class OneWeightPerBinding(variable: DefaultVariable) extends Variable {
  override def toString = variable.toString + "+"
}