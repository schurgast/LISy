/*
 * @author Stefan Schurgast
 */

package ch.uzh.ifi.ddis.markovlogicinference


import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedHashMap


/**
* Trait Grounding holds grounding with specific variable and individual. It is
* added to a extended with grounded formulas.
*/
trait Grounding {

  /* Holds root formula for grounded formula. */
  var rootFormulas: ArrayBuffer[Formula with Grounding] = ArrayBuffer()

  /* Groundings consist of variables pointing to individuals. */
  var groundings: Map[Variable, Individual]

  /**
  * Truth value whether grounding is known as evidence. If evidence for
  * grounding is found, the value is either TRUE (for evidence true) or FALSE
  * (for evidence false). If no evidence is found, the truth value is UNKNOWN.)
  */
  var evidence: TruthValue = UNKNOWN

  /**
  * Returns all grounded predicates in the grounded formula.
  *
  * @return grounded predicates of grounded formula as traversable.
  */
  def groundedPredicates: Traversable[GroundedPredicate]

  /**
  * Evaluates if a formula is true or false given the grounded predicates and
  * evidence.
  *
  * @return boolean value true if formula evaluates to true, false otherwise.
  */
  def eval: Boolean
  
  /**
  * Promotes root formula to this grounded formula and stores it as root formula.
  */
  def promoteRoot

  /*
   * Represents a truth table for the grounding.
   */
  def getEvidenceTruthMap: TruthFactor
  
  /*
   * Represents the joint distribution table
   */
  def getEvidenceWeightMap: BeliefFactor = {
    val weight = this match {
      case gwf: GroundedWeightedFormula => gwf.weight
      case _ => Double.MaxValue
    }
    val evidenceTruthMap = getEvidenceTruthMap
    //println("evidence: "+ evidenceTruthMap)
    var result: BeliefFactor = new BeliefFactor()
    
    for (key <- evidenceTruthMap.keys) {
      evidenceTruthMap(key) match {
        case None => result+=((key,None))
        case Some(booleanValue) => {
          val doubleValue = if (booleanValue) 1.0 else 0.0
          result+=((key, Some(math.exp(weight*doubleValue)))) // TODO: weight should be normalized in order to prevent too high values resulting in "infinity"
        }
      }      
    }
    result
  }  
}

/**
* Exception that is thrown when no suitable grounding is found to do
* grounding with.
*
* @param m message as string thrown with exception
*/
class NoCorrepondingGroundingException(m: String) extends Exception(m)

/**
* Class GroundedWeightedFormula represents a grounded weighted formula
* and consists of a formula WeightedFormula as well as the corresponding grounding.
*
* @param  f           weighted formula to be grounded
* @param  groundings  groundings is a map of variables and individuals with which formulas are grounded
* @param  rootFormula is the origin formula with recursive grouding process. Will be initialized with None if empty.
*/
class GroundedWeightedFormula(
  f: WeightedFormula,
  var groundings: Map[Variable, Individual],
  var rootFormula: Option[Formula with Grounding] = None
) extends WeightedFormula(f, f.weight, f.hard) with Grounding
{

  /**
  * Returns grounded weighted formula description as string.
  *
  * @return  grounded weighted formula description as string
  */
  override def toString = {
    toString(f.f.grounded(groundings).toString)
  }

  /**
  * Returns all grounded predicates in grounded weighted formula.
  *
  * @return  grounded predicates of formula
  */
  def groundedPredicates = f.f.grounded(groundings).groundedPredicates

  /**
  * Evaluates grounded weighted formula to true or false respectively.
  *
  * @return true if formula with groundings is true, false otherwise.
  */
  def eval: Boolean = {
    val result = f.f.grounded(groundings).eval
    result
  }

  /*
   * Returns truth table for the given groundings.
   * 
   * @return truth table of type TruthFactor
   */
  def getEvidenceTruthMap: TruthFactor = f.f.grounded(groundings).getEvidenceTruthMap
  
  /**
  * Promotes root formula to this grounded formula and stores it as root formula.
  */
  def promoteRoot = {
    groundedPredicates.foreach(gp => gp.rootFormulas.append(this))
  }
}

/**
* Class GroundedEquivalent represents a grounded formula of type equivalent (<->)
* and consists of a formula Equivalent as well as the corresponding grounding.
*
* @param  f           equivalent formula to be grounded
* @param  groundings  groundings is a map of variables and individuals with which formulas are grounded
* @param  rootFormula is the origin formula with recursive grouding process. Will be initialized with None if empty.
*/
class GroundedEquivalent(
  f: Equivalent,
  var groundings: Map[Variable,Individual],
  var rootFormula: Option[Formula with Grounding] = None
) extends Equivalent(f.lhs, f.rhs) with Grounding
{

  /**
  * Returns grounded equivalent description as string.
  *
  * @return  grounded equivalent description as string
  */
  override def toString = {
    //println("get this stuff here in groundedequivalent: " + f.getVars + " " + groundings)
    toString(f.lhs.grounded(groundings).toString, f.rhs.grounded(groundings).toString)
  }

  /**
  * Returns all grounded predicates in grounded equivalent by merging groundings
  * of left and right side of formula.
  *
  * @return  grounded predicates of formula
  */
  def groundedPredicates = new Traversable[GroundedPredicate] {
    def foreach[U](gp: GroundedPredicate => U) = {
      f.lhs.grounded(groundings).groundedPredicates.foreach(gp(_))
      f.rhs.grounded(groundings).groundedPredicates.foreach(gp(_))
    }
  }

  /**
  * Evaluates grounded equivalent to true or false respectively for both sides
  * and evaluates joint.
  *
  * @return true if formula with groundings is true, false otherwise.
  */
  def eval: Boolean = (lhs.grounded(groundings).eval == rhs.grounded(groundings).eval)

  /*
   * Returns the joint truth table for the given groundings.
   * 
   * @return truth table of type TruthFactor
   */
  def getEvidenceTruthMap: TruthFactor = {
    val leftFactor: TruthFactor = lhs.grounded(groundings).getEvidenceTruthMap
    val rightFactor: TruthFactor = rhs.grounded(groundings).getEvidenceTruthMap
    leftFactor <-> rightFactor
  }
  
  /**
  * Promotes root formula to this grounded formula and stores it as root formula.
  */
  def promoteRoot = groundedPredicates.foreach(gp => gp.rootFormulas.append(this))
}

/**
* Class GroundedImplies represents a grounded formula of type implies (->)
* and consists of a formula Implies as well as the corresponding grounding.
*
* @param  f           Implies formula to be grounded
* @param  groundings  groundings is a map of variables and individuals with which formulas are grounded
* @param  rootFormula is the origin formula with recursive grouding process. Will be initialized with None if empty.
*/
class GroundedImplies(f: Implies, var groundings: Map[Variable,Individual], var rootFormula: Option[Formula with Grounding] = None) extends Implies(f.lhs, f.rhs) with Grounding{
  
  /**
  * Returns grounded implication description as string.
  *
  * @return  grounded implication description as string
  */
  override def toString = toString(f.lhs.grounded(groundings).toString, f.rhs.grounded(groundings).toString)

  /**
  * Returns all grounded predicates in grounded implies by merging groundings
  * of left and right side of formula.
  *
  * @return  grounded predicates of formula
  */
  def groundedPredicates = new Traversable[GroundedPredicate] {
    def foreach[U](gp: GroundedPredicate => U) = {
      f.lhs.grounded(groundings).groundedPredicates.foreach(gp(_))
      f.rhs.grounded(groundings).groundedPredicates.foreach(gp(_))
    }
  }

  /**
  * Evaluates grounded implies to true or false respectively for both sides
  * and evaluates joint.
  *
  * @return true if formula with groundings is true, false otherwise.
  */
  def eval: Boolean = (!lhs.grounded(groundings).eval || rhs.grounded(groundings).eval)

 /*
   * Returns the joint truth table for the given groundings.
   * 
   * @return truth table of type TruthFactor
   */
  def getEvidenceTruthMap:  TruthFactor = {
    val leftFactor: TruthFactor = lhs.grounded(groundings).getEvidenceTruthMap
    val rightFactor: TruthFactor = rhs.grounded(groundings).getEvidenceTruthMap
    val truthFactor = leftFactor -> rightFactor
    truthFactor
  }
 
  /**
  * Promotes root formula to this grounded formula and stores it as root formula.
  */
  def promoteRoot = groundedPredicates.foreach(gp => gp.rootFormulas.append(this))
}

/**
* Class GroundedOr represents a grounded formula of type or (v)
* and consists of a formula Or as well as the corresponding grounding.
*
* @param  f           Or formula to be grounded
* @param  groundings  groundings is a map of variables and individuals with which formulas are grounded
* @param  rootFormula is the origin formula with recursive grouding process. Will be initialized with None if empty.
*/
class GroundedOr(f: Or, var groundings: Map[Variable, Individual], var rootFormula: Option[Formula with Grounding] = None) extends Or(f.lhs, f.rhs) with Grounding{

  /**
  * Returns grounded or description as string.
  *
  * @return  grounded or description as string
  */
  override def toString = toString(f.lhs.grounded(groundings).toString,f.rhs.grounded(groundings).toString)

  /**
  * Returns all grounded predicates in grounded or by merging all groundings of
  * disjunction.
  *
  * @return  grounded predicates of formula
  */
  def groundedPredicates = new Traversable[GroundedPredicate] {
    def foreach[U](gp: GroundedPredicate => U) = {
      f.lhs.grounded(groundings).groundedPredicates.foreach(gp(_))
      f.rhs.grounded(groundings).groundedPredicates.foreach(gp(_))
    }
  }

  /**
  * Evaluates grounded or to true or false respectively for all disjuncted
  * formulas and evaluates disjunction.
  *
  * @return true if formula with groundings is true, false otherwise.
  */
  def eval: Boolean = (lhs.grounded(groundings).eval || rhs.grounded(groundings).eval)
 
  /*
   * Returns the joint truth table for the given groundings.
   * 
   * @return truth table of type TruthFactor
   */
  def getEvidenceTruthMap:  TruthFactor = {
    val leftFactor: TruthFactor = lhs.grounded(groundings).getEvidenceTruthMap
    val rightFactor: TruthFactor = rhs.grounded(groundings).getEvidenceTruthMap
    leftFactor || rightFactor
  }
  
  /**
  * Promotes root formula to this grounded formula and stores it as root formula.
  */
  def promoteRoot = groundedPredicates.foreach(gp => gp.rootFormulas.append(this))
}

/**
* Class GroundedAnd represents a grounded formula of type and ( ^ )
* and consists of a formula And as well as the corresponding grounding.
*
* @param  f           and formula to be grounded
* @param  groundings  groundings is a map of variables and individuals with which formulas are grounded
* @param  rootFormula is the origin formula with recursive grouding process. Will be initialized with None if empty.
*/
class GroundedAnd(f: And, var groundings: Map[Variable, Individual], var rootFormula: Option[Formula with Grounding] = None) extends And(f.lhs, f.rhs) with Grounding{

  /**
  * Returns grounded and description as string.
  *
  * @return  grounded and description as string
  */
  override def toString = toString(f.lhs.grounded(groundings).toString,f.rhs.grounded(groundings).toString)

  /**
  * Returns all grounded predicates in grounded and by merging all groundings of
  * conjunction.
  *
  * @return  grounded predicates of formula
  */
  def groundedPredicates = new Traversable[GroundedPredicate] {
    def foreach[U](gp: GroundedPredicate => U) = {
      f.lhs.grounded(groundings).groundedPredicates.foreach(gp(_))
      f.rhs.grounded(groundings).groundedPredicates.foreach(gp(_))
    }
  }

  /**
  * Evaluates grounded and to true or false respectively for all conjunctions
  * formulas and evaluates conjunction.
  *
  * @return true if formula with groundings is true, false otherwise.
  */
  def eval: Boolean = (lhs.grounded(groundings).eval && rhs.grounded(groundings).eval)

  /*
   * Returns the joint truth table for the given groundings.
   * 
   * @return truth table of type TruthFactor
   */
  def getEvidenceTruthMap:  TruthFactor = { //Map[Map[GroundedPredicate, Boolean], Option[Boolean]] = {
    val leftFactor: TruthFactor = f.lhs.grounded(groundings).getEvidenceTruthMap
    val rightFactor: TruthFactor = f.rhs.grounded(groundings).getEvidenceTruthMap
    leftFactor && rightFactor
  }

  /**
  * Promotes root formula to this grounded formula and stores it as root formula.
  */
  def promoteRoot = groundedPredicates.foreach(gp => gp.rootFormulas.append(this))
}

/**
* Class GroundedNot represents a grounded formula of type not
* and consists of a formula Not as well as the corresponding grounding.
*
* @param  f           Not-formula to be grounded
* @param  groundings  groundings is a map of variables and individuals with which formulas are grounded
* @param  rootFormula is the origin formula with recursive grouding process. Will be initialized with None if empty.
*/
class GroundedNot(f: Not, var groundings:Map[Variable,Individual], var rootFormula: Option[Formula with Grounding] = None) extends Not(f) with Grounding{

  /**
  * Returns grounded not description as string.
  *
  * @return  grounded not description as string
  */
  override def toString = toString(f.f.grounded(groundings).toString)

  /**
  * Returns all grounded predicates in grounded not.
  *
  * @return  grounded predicates of formula
  */
  def groundedPredicates = f.f.grounded(groundings).groundedPredicates

  /**
  * Evaluates grounded not to true or false respectively for grounded formula.
  *
  * @return true if formula with groundings is true, false otherwise.
  */
  def eval: Boolean = !f.f.grounded(groundings).eval
 
  /*
   * Returns the joint truth table for the given groundings.
   * 
   * @return truth table of type TruthFactor
   */
  def getEvidenceTruthMap: TruthFactor = {
    val dummy = TruthFactor(f.f.grounded(groundings).getEvidenceTruthMap)
    dummy!(f.f.grounded(groundings).getEvidenceTruthMap) // TODO: not really nice
  }
    
  /**
  * Promotes root formula to this grounded formula and stores it as root formula.
  */
  def promoteRoot = groundedPredicates.foreach(gp => gp.rootFormulas.append(this))
}

/**
* Class <code>GroundedPredicate</code> represents a grounded predicate and 
* consists of a <code>Predicate</code> as well as the corresponding
* <code>Grounding</code>.
*
* @param  f           predicate to be grounded
* @param  groundings  groundings is a map of variables and individuals with 
*                     which predicates are grounded
* @param  rootFormula is the origin formula with recursive grouding process. 
*                     Will be initialized with <code>None</code> if empty.
*/
class GroundedPredicate(
  val f: Predicate,
  var groundings: Map[Variable, Individual],
  var rootFormula: Option[Formula with Grounding] = None,
  val mln: MarkovLogicNetwork = ObjMln
) extends Predicate(f.name) with Grounding
{
  self =>

  override def getVars = f.getVars

  // predicate adds itself to MLN
  mln.addGroundedPredicate(this)

  /**
  * Promotes root formula to this grounded predicate and stores it as root formula.
  */
  def promoteRoot = groundedPredicates.foreach(gp => gp.rootFormulas.append(this))

  /**
  * Returns grounded predicate description as string.
  *
  * @return  grounded predicate description as string
  */
  override def toString = { f.vars.map(groundings.get(_)) match {
      case List(None) => throw new NoCorrepondingGroundingException("There is no correct grounding for predicate " + this + " (groundings: " + groundings + ").")
      case _ => toString(f.vars.map{x =>  groundings.get(x).get.toString}) //ObjMln.prettyString(groundings.get(x).get.toString)})
    }
  }

  /**
  * Returns all grounded predicates of predicate with grounding.
  *
  * @return  grounded predicates of predicate with grounding.
  */
  def groundedPredicates = new Traversable[GroundedPredicate] {
    def foreach[U](gp: GroundedPredicate => U) = gp(self)
  }

  /* Holds value for trueness of predicate. Can be updated by reasoning algorithm. */
  var evaluationValue = 0.8

  /**
  * Evaluates predicate to true or false depending on truth value distribution.
  * If evaluation value updated by reasoning algorithm > 0.5 then true, false
  * otherwise.
  *
  * @return true if predicate evaluates to true, false otherwise.
  */
 
  def eval: Boolean = {
    val b = if (evidence == TRUE) true
    else if (evidence == FALSE) false
    else if (evaluationValue > 0.5) true
    else false
    b
  }
  
  /*
   * Returns the truth map of type TruthFactor for a predicate.
   * 
   * In case of evidence, the non-possible value is defined as None.
   * 
   * There are values for both, potentially true and potentially false which are 
   * both valid values for a predicate. This is important when joining the truth
   * tables further up in the formulas in order to propagate evidence. The true
   * and false values are also used for marginalization and normalization later
   * on.
   * 
   * @return truth table of type TruthFactor
   */
  def getEvidenceTruthMap: TruthFactor = {
    var result: TruthFactor = new TruthFactor()
    evidence match {
      case TRUE => {
          result.put(LinkedHashMap(this -> true), Some(true))
          result.put(LinkedHashMap(this -> false), None)
      }
      case FALSE => {
          result.put(LinkedHashMap(this -> true), None)
          result.put(LinkedHashMap(this -> false), Some(false))
      }
      case _ => {
          result.put(LinkedHashMap(this -> true), Some(true))
          result.put(LinkedHashMap(this -> false), Some(false))
      }
    }
    //println("evidence: " + this.toString + " " + evidence + " result: " + result)
    result
  }
}