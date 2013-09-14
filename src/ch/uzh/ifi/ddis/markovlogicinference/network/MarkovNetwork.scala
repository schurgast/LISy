/*
 * @author Stefan Schurgast
 */

package ch.uzh.ifi.ddis.markovlogicinference

import ch.uzh.ifi.ddis.signalcollect.Bindings
import ch.uzh.ifi.ddis.signalcollect.Logging
import ch.uzh.ifi.ddis.signalcollect.SparqlAccessor
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedHashMap

/*
 * Object representing the Markov network on which reasoning is done.
 * 
 * TODO: this should be eliminated in the long run, passing the MLN as parameter.
 */
object ObjMln extends MarkovLogicNetwork {
  def reset = {
    groundedPredicates = LinkedHashMap()
    formulas = ArrayBuffer()
    groundedFormulas = ArrayBuffer()
    predicateGroundings = ArrayBuffer()
    predicateCount = 0
  }
}

/*
 * This class represents the Markov network on which reasoning is implemented on.
 * It contains formulas and can ground them, as well as provide the grounded
 * predicates and formulas for the factor graph.
 */
class MarkovLogicNetwork() extends Logging {
  
  /*
   * represents all possible truth values
   */
  //val truthValues = (TRUE :: FALSE :: Nil) 
  
  /*
   * Lookup map for grounded predicates. This is how the predicates know if 
   * they should generate a new grounded predicate or use an existing one.
   */
  var groundedPredicates: LinkedHashMap[String,LinkedHashMap[List[String],GroundedPredicate]] = LinkedHashMap()
  
  /*
   * Formulas in Markov logic.
   */
  var formulas:ArrayBuffer[Formula] = ArrayBuffer()
  
  /*
   * Holds all grounded formulas of the network.
   */
  var groundedFormulas: ArrayBuffer[Formula with Grounding] = ArrayBuffer()
  
  /*
   * Holds all grounded predicates of the network
   */
  var predicateGroundings: ArrayBuffer[Formula with Grounding] = ArrayBuffer()
  
  /*
   * Counts the predicates for statistics
   */
  var predicateCount:Int = 0
  
  /*
   * Displays the string in readable format, removing the URL
   */
  def prettyString(s: String) = {
    val splitted = s.split("#")
    if (splitted.size == 2)
      splitted(1)
    else
      s
  }
  
  /*
   * Prints all grounded formulas of the markov network with the corresponding
   * truth tables.
   */
  def printMarkovNetwork {
    for (formula <- ObjMln.groundedFormulas) {
      println(formula + " " + formula.getEvidenceTruthMap)
    }
  }
  
  /**
   * initialize MLN
   * (1) grounds all formulas that have been added to MLN
   * (2) retrieves evidence
   */
  def init(db: SparqlAccessor) {
    this.ground(db)
    this.getEvidence(db) 
    //printMarkovNetwork
    log(ObjMln.predicateCount + " predicates and " + ObjMln.groundedFormulas.size + " formulas grounded.")
  }

  /*
   * Holds the query prefix for the SPARQL query
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
  "PREFIX testingowl2:<http://www.semanticweb.org/ontologies/2010/4/testingowl2.owl#>\n"
  
  /*
   * Returns the evidence query
   */
  val evidenceQuery = queryPrefix +
  " select ?individual ?property ?value WHERE {\n" +
  "?property rdfs:subPropertyOf ?object .\n" +
  "?individual ?property ?value\n" +
  "}"
  
  /*
   * Binds all the evidence for a given database to the corresponding grounded
   * predicates.
   * 
   * @param db    SPARQL endpoint of type SparqlAccessor
   */
  def getEvidence(db: SparqlAccessor) = {
    val bindings: Traversable[Bindings] = db.execute(evidenceQuery)
    for (binding <- bindings) {
      val individual = binding.get("individual") // individual
      val property = binding.get("property") // e.g. smokes
      val value = binding.get("value") // true,ndividual = binding.get("individu false resp. range value
      value match {
        case Some(FALSE.rdfString) => groundedPredicates.get(property.get).get(List(individual.get)).evidence = FALSE
        case Some(TRUE.rdfString) => groundedPredicates.get(property.get).get(List(individual.get)).evidence = TRUE
        case Some(_) => groundedPredicates.get(property.get).get(List(individual.get, value.get)).evidence = TRUE
        case None => throw new Exception("not supposed to happen with evidence")
      } 
    }
  }
  
  /*
   * Adds a new formula to the network.
   * 
   * @param f   formula to be added to the network
   */
  def addFormula(f:Formula) = formulas.append(f)
  
  /*
   * Ground entire network with the data from the sparql endpoint.
   */
  def ground(db: SparqlAccessor): ArrayBuffer[Formula with Grounding] = {
    for(f <- formulas) {
      val g = f.ground(db)
      groundedFormulas.appendAll(g)
      g.foreach(_.promoteRoot) // TODO: promote root crashes with wrong groundings in grounded formulas
    }
    groundedFormulas
  }
  
  /*
   * Prints all the grounded predicates.
   */
  def printGroundedPredicates {
    var counter = 0
    for (gp <- predicateGroundings) {
      counter = counter+1
      println(counter + " predicate(s): "+gp) 
    }
  }
  
  /*
   * Prints all the grounded formulas.
   */
  def printGroundedFormulas {
    var counter = 0
    for (gf <- groundedFormulas) {
      counter = counter+1
      println(counter + " formulas(s): "+gf) 
    }
  }

  /**
   * adds grounded predicate to MLN (in predicate list as well as in predicate lookup)
   * GroundedPredicates add themselves to MLN Map groundedPredicates on creation
   * this list is used to generate variable nodes in reasoning algorithms
   */
  def addGroundedPredicate(gp: GroundedPredicate) = {
    predicateGroundings+=gp // add to predicate list
    //if (predicateCount % 1000 == 0) println(predicateCount)
    gp.f.vars.map(gp.groundings.get(_)) match { // check if for variable corresponding individuals exist.
      case List(None) => throw new Exception("Grounded predicate with variable(s) " + gp.f.vars + " didn't get correct groundings ("+ gp.groundings +"). So it cannot be added to MLN.")
      case _ => {
          val individualList: List[String] = gp.f.vars.map(gp.groundings.get(_).get.name)
          groundedPredicates.get(gp.name) match  { // check if predicate entry already exists
            case Some(individuals) => individuals.put(individualList,gp) // add new individuals with predicate groundings to predicate lookup
            case None => {
                val individuals: LinkedHashMap[List[String],GroundedPredicate] = LinkedHashMap(individualList -> gp)
                groundedPredicates.put(gp.name,individuals) // add to predicate lookup
              }
          }
        }
    }
  }
}