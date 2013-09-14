/*
 *  @author Philip Stutz & Stefan Schurgast
 *  @version %I%, %G%
 */

package ch.uzh.ifi.ddis.markovlogicinference

import ch.uzh.ifi.ddis.signalcollect.SparqlAccessor
import scala.collection.immutable.List
import scala.collection.mutable.HashSet

/*
 * open issues:
 * ------------
 * 
 * - the quantifiers are not implemented yet.
 * - the SPARQL query prefix is still in this class and must be adopted to 
 *   other datasets if they shall be used. Move the prefix to some central 
 *   configuration spot in the network.
 */

/*
 * TODO: the unversial and the existential quantifier for first-order logic
 * are not implemented yet. Skolonization is needed.
 */
abstract class FirstOrderLogic() {

  def ∀(x: Variable, f: Formula) = f
  def forall(x: Variable, f: Formula) = f

  def exists(x: Variable, f:Formula) = f
  def ∃(x: Variable, f:Formula) = f  
}

/**
* Abstract class Formula represents a formula in its very general form. Formulas
* are built of atoms (like TRUE and FALSE), predicates and logical connectives.
* Also subformulas may be used.
* <p>
* With the logical connectives defined as methods, formulas can be constructed
* within the code in their natural represenation
* (e.g. Smokes(Hans) -> HasCancer(Hans))
*/
abstract class Formula { //extends Logging {

  /*
   * The vars define the variables in the formula, e.g. smokes(x) or friends(x,y)
   */
  def getVars: List[Variable]
  
  /**
  * If a formula contains a predicate like "Smokes"~(x), a new predicate of type
  * Predicate is created.
  *
  * @return  Predicate with name of string
  */
  implicit def string2predicate(x: String) = new Predicate(x)
  
  /**
  * If formula contains a not with some formula or predicate, method returns a
  * new Not formula.
  *
  * @return  Not formula with content formula
  */
  def ¬(f: Formula) = new Not(f)
  def not(f: Formula) = new Not(f)
  
  /**
  * symbol ^ creates new <code>And</code> formula with this as left hand side
  * and that as right hand side.
  *
  * @param   that represents right hand side formula
  * @return  <code>And</code> formula
  */
  def ^(that: Formula) = new And(this,that)
  def ∧(that: Formula) = new And(this,that)

  /**
  * symbol <-> creates new <code>Equivalent</code> formula with this as left
  * hand side and that as right hand side.
  *
  * @param   that represents right hand side formula
  * @return  <code>Equivalent</code> formula
  */
  def <->(that: Formula) = new Equivalent(this,that)

  /**
  * symbol v creates new <code>Or</code> formula with this as left hand side
  * and that as right hand side.
  *
  * @param   that represents right hand side formula
  * @return  <code<Or</code> formula
  */
  def v(that: Formula) = new Or(this,that)
  def ∨(that: Formula) = new Or(this,that)

  /**
  * symbol -> creates new <code>Implies</code> formula with this as left
  * hand side and that as right hand side.
  *
  * As alternative to Implies one could also use
  * def ->(that: Formula) = new Or(List(new Not(that),this))
  *
  * @param   that represents right hand side formula
  * @return  <code>Implies</code> formula
  */
  def ->(that: Formula) = new Implies(this,that)
  def →(that: Formula) = new Implies(this,that)
  
  /**
  * symbol w creates new <code>Weighted</code> formula. The weight w (Double) is
  * used in Markov Logic in order to use logic and probabilistic reasoning in the
  * same model.
  *
  * @param   weight weight of a formula
  * @return  new weighted formula
  */
  def w(weight: Double) = new WeightedFormula(this, weight)

  /**
  * Grounds formula recursively. See predicates, and, or, not, implies, equivalent
  * and weighted formula for specific implementation.
  *
  * @param   db          some sparql accessor (e.g. sesame sparql)
  * @param   groundings  contains map from variable to individual for each grounding
  * @return  grounded formulas
  */
  def ground(db: SparqlAccessor, groundings: HashSet[Map[Variable,Individual]] = HashSet[Map[Variable,Individual]]()): HashSet[Formula with Grounding]

  /**
  * Returns new grounded formula for a specific grouding. See specfic implementations.
  *
  * @param   groundings  contains map from variable to individual for each grounding
  * @return  grounded formula
  */
  def grounded(groundings: Map[Variable,Individual]): Formula with Grounding
}

/**
* Class BinaryOperatorFormula is an abstract class, that represents all binary
* operators. It has a left side <code>lhs</code> as well as a right side 
* <code>rhs</code>.
*
* @param  lhs left hand side of formula
* @param  rhs right hand side of formula
*/
abstract class BinaryOperatorFormula(val lhs: Formula, val rhs: Formula) extends Formula
{
  
  /*
   * Returns all the variables of the formula, e.g. smokes(x) -> cancer(y)
   * 
   * @return list of variables of the formula
   */
  def getVars = lhs.getVars ::: rhs.getVars

  /* logical connective */
  val op: String

  /**
  * Returns operator description as string (containing the formula description 
  * including all subformulas in right hand side and left hand side). This 
  * method implements the general case.
  *
  * @return  formula description as string
  */
  override def toString = toString(lhs.toString, rhs.toString)

  /**
  * Connects left and right hand side with operator. Used for toString method.
  *
  * @param   lhs left hand side subformula string
  * @param   rhs right hand side subformula string
  *
  * @return  formula description as string
  */
   def toString(lhs: String, rhs: String) = "(" + lhs + op + rhs + ")"

  /**
  * Grounds binary formula recursively.
  * <p>
  * Each side is grounded seperately and then merged to the corresponding
  * binary formula.
  *
  * @param   db          some sparql accessor (e.g. sesame sparql)
  * @param   groundings  contains map from variable to individual for each grounding
  *
  * @return  grounded equivalent formulas of type GroundedEquivalent
  *
  * @see     GroundedEquivalent
  */
  override def ground(
    db: SparqlAccessor,
    groundings:HashSet[Map[Variable,Individual]]
  ): HashSet[Formula with Grounding] =
  {
    var result:HashSet[Formula with Grounding] = HashSet[Formula with Grounding]() // empty result
    val leftGroundedFormulas: HashSet[Formula with Grounding] = lhs.ground(db, groundings)  //returns all possible groundings for left side of the formula
    val rightGroundedFormulas: HashSet[Formula with Grounding] = rhs.ground(db, leftGroundedFormulas.map(_.groundings)) //returns all possible groundings for right side of the formula without having to ground variables that have been grounded in already in lhs again
/*
    println("lhs: " + lhs + " " + groundings)
    println("leftGroundedFormulas: "+leftGroundedFormulas)
    println("lhs: " + rhs)
    println("rightGroundedFormulas: "+rightGroundedFormulas)
*/    
    for (left <- leftGroundedFormulas) {
      for (right <- rightGroundedFormulas) {
        val groundedFormula = grounded(left.groundings.++(right.groundings))
        result.add(groundedFormula)
      }
    }
//    println(result)
    result
  }
}

/**
* Class WeightedFormula represents a formula a weight
*
* @param  f       formula
* @param  weight  weight of formula of type Double
* @param  hard    defines of formula is hard or weighted
*/
class WeightedFormula(val f: Formula, val weight: Double = 0, val hard: Boolean = false) extends Formula
{
  def getVars = f.getVars

  /**
  * Connects formula with weight or "." for hard respectively. Used for toString method.
  *
  * @param   formula    string representing formula
  *
  * @return  string representation of weighted formula
  */
  def toString(formula:String) = if (hard) formula + "." else formula + " " + weight

  /**
  * Returns weighted formula description as string (containing weighted formula
  * with weight).
  *
  * @return  weighted formula description as string
  */
  override def toString = toString(f.toString)

  /**
  * Grounds weighted formula recursively.
  *
  * @param   db          some sparql accessor (e.g. sesame sparql)
  * @param   groundings  contains map from variable to individual for each grounding
  *
  * @return  grounded weighted formulas
  *
  * @see     GroundedWeightedFormula
  * @see     Grounding
  */
  override def ground(db: SparqlAccessor, groundings:HashSet[Map[Variable,Individual]]): HashSet[Formula with Grounding] = {
     var result:HashSet[Formula with Grounding] = HashSet()
 //    println(groundings)
     val allGroundedFormulas = f.ground(db, groundings).map(_.groundings)
     for (g <- allGroundedFormulas) {
      result.add(grounded(g))
     }
//     println("result: " + result)
     result
  }

  /**
  * Returns new grounded weighted formula for a specific grounding.
  *
  * @param   groundings  contains map from variable to individual for each grounding
  * @return  grounded weighted formula
  */
  override def grounded(groundings: Map[Variable,Individual]): Formula with Grounding = new GroundedWeightedFormula(this,groundings)
}


/**
* Class Equivalent represents the logic connective biconditional (if and only
* if). It has a left side <code>lhs</code> as well as a right side <code>rhs</code>.
*
* @param  lhs left hand side of formula
* @param  rhs right hand side of formula
*/
class Equivalent(lhs: Formula, rhs: Formula) extends BinaryOperatorFormula(lhs, rhs)
{
  /* logical connective representation for biconditional is <-> */
  override val op = " <-> "

  /**
  * Returns new grounded equivalent of type GroundedEquivalent for a specific
  * grounding.
  *
  * @param   groundings  contains map from variable to individual for each grounding
  *
  * @return  grounded equivalent formula of type GroundedEquivalent
  */
  override def grounded(groundings: Map[Variable,Individual]): Formula with Grounding = new GroundedEquivalent(this,groundings)
}

/**
* Class Implies represents the logic connective implication (if...then).
* It has a left side <code>lhs</code> as well as a right side <code>rhs</code>.
*
* @param  lhs left hand side of formula
* @param  rhs right hand side of formula
*/
class Implies(lhs: Formula, rhs: Formula) extends BinaryOperatorFormula(lhs, rhs)
{
  /* logical connective representation for implication is -> */
  override val op = " -> "

  /**
  * Returns new grounded implication of type GroundedImplies for a specific
  * grounding.
  *
  * @param   groundings  contains map from variable to individual for each grounding
  *
  * @return  grounded implies formula of type GroundedImplies
  */
  override def grounded(groundings: Map[Variable,Individual]): Formula with Grounding = new GroundedImplies(this,groundings)
}

/**
* Class Or represents the logic connective or.
*
* @param  disjunction  holds all disjoint formulas
*/
class Or(lhs: Formula, rhs: Formula) extends BinaryOperatorFormula(lhs, rhs)
{
   /* logical connective representation for or is v */
  override val op = " v "

  /**
  * Returns new grounded or of type GroundedOr for a specific grounding.
  *
  * @param   groundings  contains map from variable to individual for each grounding
  *
  * @return  grounded or formula of type GroundedOr
  */
  override def grounded(groundings: Map[Variable,Individual]): Formula with Grounding = new GroundedOr(this,groundings)
}

/**
* Class And represents the logic connective and.
*
* @param  conjunction  holds all conjoint formulas
*/
class And(lhs: Formula, rhs: Formula) extends BinaryOperatorFormula(lhs, rhs)
{
  /* logical connective representation for and is ^ */
  val op = " ^ "

  /**
  * Returns new grounded and of type GroundedAnd for a specific grounding.
  *
  * @param   groundings  contains map from variable to individual for each grounding
  *
  * @return  grounded and formula of type GroundedAnd
  */
  override def grounded(groundings: Map[Variable,Individual]): Formula with Grounding = new GroundedAnd(this,groundings)
}

/**
* Class Not represents the logic connective not.
*
* @param  f  formula to be negated
*/
class Not(val f: Formula) extends Formula
{
  def getVars = f.getVars

  /* logical connective representation for not is not */
  val op = "not"

  /**
  * Connects formula with negation operator. Used for toString method.
  *
  * @param   f represent subformula as string
  *
  * @return  not description as string
  */
  def toString(formula:String) = op + "(" + formula + ")"

  /**
  * Returns not description as string (containing not formula
  * description for subformula).
  *
  * @return  not description as string
  */
  override def toString = toString(f.toString)

  /**
  * Grounds not formula recursively.
  * <p>
  * Each element of conjunction is grounded seperately and then merged to not formula.
  *
  * @param   db          some sparql accessor (e.g. sesame sparql)
  * @param   groundings  contains map from variable to individual for each grounding
  *
  * @return  grounded not formulas of type GroundedNot
  *
  * @see     GroundedNot
  */
  override def ground(db: SparqlAccessor, groundings: HashSet[Map[Variable, Individual]]):HashSet[Formula with Grounding] = {
    var result: HashSet[Formula with Grounding] = HashSet()
      val allGroundings = f.ground(db, groundings).map(_.groundings)
      for (g <- allGroundings) {
      result.add(grounded(g))
    }
    result
  }

  /**
  * Returns new grounded not of type GroundedNot for a specific grounding.
  *
  * @param   groundings  contains map from variable to individual for each grounding
  *
  * @return  grounded not formula of type GroundedNot
  */
  override def grounded(groundings: Map[Variable,Individual]): Formula with Grounding = new GroundedNot(this,groundings)
}

/**
* Class Predicate represents a predicate with one or two variables e.g. Friends(x,y)
*
* @param  name  name of predicate as string is key to predicate.
*/
class Predicate(val name: String) extends Formula {

  /**
  * vars contain variables of predicate
  * e.g. x and y for predicate Friends(x,y)
  */
  var vars: List[Variable] = Nil
  def getVars = vars

  /**
  * Short form for binding variable to predicate (e.g. "Smokes"~(x))
  *
  * @param  x variable that is bound with predicate
  * @return predicate with name (this) and variables
  */
  def ~(x: Variable*): Predicate = this.bind(x.toList)

  /**
  * Binds variable to predicate (e.g. "Smokes"~(x))
  *
  * @param  x variable that is bound with predicate
  * @return predicate with name (this) and variables
  */
  def bind(v: List[Variable]): Predicate = {
    vars = v
    this
  }

  /**
  * sparql query prefix for owl ontology in rdf store
  *
  * TODO: query prefix specific for testingowl2 data set. make that more general.
  */
  val queryPrefix = "PREFIX sqwrl:<http://sqwrl.stanford.edu/ontologies/built-ins/3.4/sqwrl.owl#>\n" +
                    "PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>\n" +
                    "PREFIX swrl:<http://www.w3.org/2003/11/swrl#>\n" +
                    "PREFIX owl2xml:<http://www.w3.org/2006/12/owl2-xml#>\n" +
                    "PREFIX protege:<http://protege.stanford.edu/plugins/owl/protege#>\n" +
                    "PREFIX xsp:<http://www.owl-ontologies.com/2005/08/07/xsp.owl#>\n" +
                    "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>\n" +
                    "PREFIX owl:<http://www.w3.org/2002/07/owl#>\n" +
                    "PREFIX swrlb:<http://www.w3.org/2003/11/swrlb#>\n" +
                    "PREFIX j.0:<http://www.owl-ontologies.com/>\n" +
                    "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
                    "PREFIX swrla:<http://swrl.stanford.edu/ontologies/3.3/swrla.owl#>\n" +
                    "PREFIX testingowl2:<http://www.semanticweb.org/ontologies/2010/4/testingowl2.owl#>\n" // TODO: make this part general, no hard coding!


  /**
  * Generates predicate sparql query for OWL ontology in RDF store. Query selects
  * all individuals in store that are in domain respectivelly range of specific
  * predicate. Query works for unary and binary predicate groundings.
  *
  * @return generated sparql query string
  */
  def generatePredicateQuery: String = {
    var predicateQueryFilter = ""
    // for same variables, identical individuals are fetched
    if ((vars.size > 1) && (vars(0)==vars(1))) { predicateQueryFilter = " FILTER (?i1 = ?i2) " }
    val predicateQuery = queryPrefix + " select ?i1 ?i2 WHERE {\n" +
                        "<" + name + ">" + " rdfs:domain ?class .\n" +
                        "?i1 rdf:type ?class \n" +
                        " OPTIONAL {\n" +
                        "<" + name + ">" + " rdfs:range ?class .\n" +
                        "   ?i2 rdf:type ?class\n" +
                        " }\n" + predicateQueryFilter +
                        "}\n"
                      //  println(predicateQuery)
    predicateQuery
  }

  /**
  * Gets grounded predicate for specific grounding.
  * <p>
  * Creates new grounded predicate. If predicate already exists in MLN, 
  * new grounding is added and returned. add grounding to existing grounded
  * predicate and return it.
  *
  * @param      groundings contains map from variable to individual for each grounding
  * @return     grounded   predicate
  *
  * @exception  Exception  if grounding doesn't match to predicate
  *
  * @see        GroundedPredicate
  */
  def getGroundedPredicate(groundings:Map[Variable, Individual]): GroundedPredicate =
  {
    val mln: MarkovLogicNetwork = ObjMln

    // check if predicate with key (name) already exists. If not, create new.
    mln.groundedPredicates.get(name) match {
      case Some(gps) => {

        // security check: do all groundings exist?
        vars.map(groundings.get(_)) match {
          case List(None) => throw new Exception("Cannot ground variable " + vars + " with " + groundings + ". No such grounding in Map.") // does not detect 1 failed out of 2
          case _ => {

            // if grounded predicate already exists, then reuse. Else create new.
            val someKey = vars.map(groundings.get(_).get.name)
            val existingGp = gps.get(someKey)
            existingGp match {
              case Some(gp) => {
                // add grounding and return grounded predicate
                vars.foreach { variable => { gp.groundings = gp.groundings.+((variable,groundings(variable))) }}
                gp
              }
              case None => createNewGroundedPredicate(mln, groundings)
            }
          }
        }
      }
      case None => createNewGroundedPredicate(mln, groundings)
    }
  }

  /**
  * Creates a new grounded predicate with provided groundings.
  * <p>
  * Method creates a new object GroundedPredicate with provided groundings.
  * For statistics, the grounded predicate is counted in the MLN.
  * <p>
  * This method should only be called if the grounded predicate not already exists.
  *
  * @param   mln        MLN that contains grounded predicates as nodes
  * @param   groundings contains map from variable to individual for each grounding
  * @return  grounded predicate
  * @see     GroundedPredicate
  */
  private def createNewGroundedPredicate(
    mln: MarkovLogicNetwork,
    groundings:Map[Variable, Individual]
  ): GroundedPredicate =
  {
    mln.predicateCount += 1
    new GroundedPredicate(this, groundings)
  }

  /**
  * Returns grounded predicates for selected single slot (unary predicates).
  * <p>
  * Existing groundings get extended with new groundings, and joint groundings
  * get returned.
  *
  * @param   db         some sparql accessor (e.g. sesame sparql)
  * @param   groundings contains map from variable to individual for each grounding
  * @param   a          represents variable that stands for Individual in grounding
  *
  * @return  new grounded predicate
  *
  * @see     GroundedPredicate
  */
  private def groundSinglePredicateSlot(
    db: SparqlAccessor,
    groundings: HashSet[Map[Variable, Individual]],
    a: Variable
  ): HashSet[Formula with Grounding] =
  {
    var result: HashSet[Formula with Grounding] = HashSet()
    var currentGroundings: HashSet[Map[Variable, Individual]] = groundings

    /* If predicate with variable is not already grounded, then ground with
       database. Otherwise use already existing grounded predicate. */
    if (!groundings.map(_.contains(a)).contains(true)) { //TODO: verify that previous variable is compatible with slot restrictions (map mit grounded by, bsp. person, die schwanger ist)
      // predicate doesn't exist with grounding of variable a, get new grounding
      val bindings = db.execute(generatePredicateQuery)
      for (binding <- bindings) {
        val i1 = binding.get("i1")
        result = result.union(HashSet(getGroundedPredicate(Map(a -> Individual(i1.get)))))
        result
      }
    } else {
      // predicate already exists, use existing
      result = result.union(currentGroundings.map(getGroundedPredicate(_)))
    }
    result
  }

  // TODO: noch gleich grounden wie oben
  def groundDoublePredicateSlot(
    db: SparqlAccessor,
    groundings: HashSet[Map[Variable, Individual]],
    a: Variable,
    b: Variable
  ): HashSet[Formula with Grounding] =
  {
    var result: HashSet[Formula with Grounding] = HashSet()
    var currentGroundings: HashSet[Map[Variable, Individual]] = groundings
    
    /* If predicate with variable is not already grounded, then ground with
       database. Otherwise use already existing grounded predicate. */
    if (!(groundings.map(_.contains(a)).contains(true) || groundings.map(_.contains(b)).contains(true))) { //TODO: verify that previous variable is compatible with slot restrictions (map mit grounded by, bsp. person, die schwanger ist)
      // predicate doesn't exist with grounding of variable a, get new grounding
      val bindings = db.execute(generatePredicateQuery)
      for (binding <- bindings) {
        val i1 = binding.get("i1")
        val i2 = binding.get("i2") // TODO: Individual string should only be stored once, otherwise get existing individual
        var currentGroundings = Map(a -> Individual(i1.get)).+((b,Individual(i2.get)))
        result = result.union(HashSet(getGroundedPredicate(currentGroundings)))
        result
      }
    // TODO: if one of the two groundings is already fetched, the other one needn't be fetched too anymore.
    } else {
      // predicate already exists, use existing
      result = result.union(currentGroundings.map(getGroundedPredicate(_)))
    }
    result
  }   
  
  /**
  * Grounds predicate for a specific database <code>db</code> and given
  * groundingsand returns GroundedPredicates as an ArrayBuffer.
  * <p>
  * The resulting grounded predicate is a Predicate of type Formula extended
  * with Grounding, and represents a predicate of the database with a mapping
  * between a variable and an individual.
  * <p>
  * Only unary and binary predicates are supported.
  *
  * @param   db         some sparql accessor (e.g. sesame sparql)
  * @param   groundings contains map from variable to individual for each grounding
  *
  * @return  grounded predicate
  *
  * @see     GroundedPredicate
  */
  override def ground(
    db: SparqlAccessor,
    groundings: HashSet[Map[Variable, Individual]]
  ): HashSet[Formula with Grounding] =
  {
   var result:HashSet[Formula with Grounding] = HashSet()
   vars match {
     case List(a) => result = groundSinglePredicateSlot(db, groundings, a) // grounding with one variable: groundSinglePredicateSlot(db, groundings, a)
     case List(a,b) => result = groundDoublePredicateSlot(db, groundings, a, b) // grounding with two variables
     case _ => throw new Exception("List ist neither unary nor binary. Predicate is not supported.")
   }
//   for (res <-  result) {println("result of ground: " + res.getClass + " " + res.groundings + " " +  vars+ " " + res.getVars  )}
   result
  }

  /**
  * Builds grounded predicate description string containing name as well as
  * grounded individuals.
  *
  * @return  predicate description as string
  */
  def toString(items: Traversable[String]) = {
    val splitted = name.split("#")
    if (splitted.size == 2)
      splitted(1) + items.mkString("(", "," , ")")
    else
      name + items.mkString("(", "," , ")")
  }

  /**
  * Returns grounded predicate description as string (containing predicate 
  * description and individuals for each variable).
  *
  * @return  predicate description as string
  */
  override def toString = toString(vars.map(_.toString))

  /**
  * Returns new grounded predicate of type GroundedPredicate for a specific grounding.
  *
  * @param   groundings  contains map from variable to individual for each grounding
  *
  * @return  grounded or formula of type GroundedPredicate
  */
  override def grounded(groundings: Map[Variable,Individual]): Formula with Grounding = {
    getGroundedPredicate(groundings) // TODO: check correct usage of groundings
  }
}

/**
* Class Individual represents an individual and can be used for grounding in
* predicates. Individuals are equally definied like individuals in OWL.
*
* @param  name           name of individual
*/
case class Individual(val name: String) {

  /**
  * Returns name of individual as string
  *
  * @return  name of individual as string
  */
  override def toString = ObjMln.prettyString(name)
}