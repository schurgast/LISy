/*
 *  @author Stefan Schurgast
 */

/*
 * known bugs:
 * -----------
 * - tests halt without crashing or any message when processing sometimes
 * - sometimes the same test that ran before crashes with the following message: Exception in thread "Thread-147" java.lang.Exception: BeliefFactor multiplication failed. That does not contain Map(Smokes(F05) -> true, HasCancer(F03) -> true). That is Map(Map(Smokes(F05) -> true) -> Some(0.27565064135416967), Map(Smokes(F05) -> false) -> Some(0.7243493586458303)).
 * - 
 */


package ch.uzh.ifi.ddis.markovlogicinference

import Predef._
import ch.uzh.ifi.ddis.signalcollect.ComputeGraph
import ch.uzh.ifi.ddis.signalcollect.Logging
import ch.uzh.ifi.ddis.signalcollect.SesameSparql
import ch.uzh.ifi.ddis.signalcollect.GraphModification
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.util.Date
import org.apache.log4j.ConsoleAppender
import org.apache.log4j.FileAppender
import org.apache.log4j.Level
import org.apache.log4j.SimpleLayout
import scala.collection.JavaConversions._

/*
 * The body of the Main object defines the main program.
 */
object Main {

  /*
   * Profiler hook that is used for profiling with NetBeans profiler.
   */
  def profilerHook = {
    println("Connect the profiler and press any key to continue:")
    val in = new InputStreamReader(System.in)
    val reader = new BufferedReader(in)
    reader.readLine
    println("Starting ...")
  }

 /*
  * If formula contains a not with some formula or predicate, method returns a
  * new Not formula.
  *
  * @param   f  formula entered in Scala
  * 
  * @return  Not formula with content formula
  */
  def not(f: Formula) = new Not(f)
 
 /*
  * If formula contains a not with some formula or predicate, method returns a
  * new Not formula.
  *
  * @param   f  formula entered in Scala
  *
  * @return  Not formula with content formula
  */
  def ¬(f:Formula) = new Not(f)
  
  /*
   * value db defines the SPARQL endpoint of the connected Sesame triple store
   * 
   * @return new sesame sparql connector that can be queried for individuals
   */
  val db = new SesameSparql("http://athena.ifi.uzh.ch:8080/openrdf-sesame", "tinysmokes")
  
  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    
    // settings for debug and error logging
    val layout = new SimpleLayout
    Logging.addAppender(new ConsoleAppender(layout))
    Logging.addAppender(new FileAppender(layout, "logs/measurements.log", false))
    Logging.setLevel(Level.ERROR)
    
        
    /* this section of the main method at the time being to define evaluations */    
    
    // output header line in console    
    println("testId; processors; countGroundedPredicates; countGroundedFormulas; groundingTime; executionTime; totalTime")
    
    // number of workers to initialize at the most
    val numberOfProcessors = 2
    
    // number of cycles per evaluation test    
    val longcycle = 10
    val shortcycle = 10
    val tinycycle = 1
    
    /*
     * The following sections mark single evaluations that are repeated for 
     * multiple threads as well as for scoring/non-scoring. Each evaluation is
     * repeated depending on the number of cycles defined above (e.g. 100 times).
     */
        
    //////////////////////////////////////////////
    // A
    //////////////////////////////////////////////
    for (workers <- 1 to numberOfProcessors) {
      implicit def string2predicate(x: String) = new Predicate(x)
      val p = "http://www.semanticweb.org/ontologies/2010/4/testingowl2.owl#"
      val f1:Formula = (p+"Smokes")~(x) ^ (p+"Smokes")~(y) ^ (p+"Smokes")~(z) ^ (p+"Smokes")~(a) w 0.8
      for (test <- 1 to longcycle) {
        ObjMln.reset
        System.gc
        bpTest(workers, "A", f1)
      }
      for (test <- 1 to longcycle) {
        ObjMln.reset
        System.gc
        bpTestScoring(workers, "AS", f1)
      }
    }
    println("Done with evaluation A")
    
    //////////////////////////////////////////////
    // B
    //////////////////////////////////////////////
    for (workers <- 1 to numberOfProcessors) {
   //profilerHook

      implicit def string2predicate(x: String) = new Predicate(x)
      val p = "http://www.semanticweb.org/ontologies/2010/4/testingowl2.owl#"
      val f1:Formula = (p+"Smokes")~(x) -> (p+"HasCancer")~(x) w 0.8
      for (test <- 1 to longcycle) {
        ObjMln.reset
        System.gc
        bpTest(workers, "B", f1)
      }
      for (test <- 1 to longcycle) {
        ObjMln.reset
        System.gc
        bpTestScoring(workers, "BS", f1)
      }
    }
    println("Done with evaluation B")
    
    //////////////////////////////////////////////
    // C
    //////////////////////////////////////////////
    implicit def string2predicate(x: String) = new Predicate(x)
    for (workers <- 1 to numberOfProcessors) {
      val p = "http://www.semanticweb.org/ontologies/2010/4/testingowl2.owl#"
      val f1:Formula = (p+"Smokes")~(x) -> (p+"HasCancer")~(y) w 0.8
      for (test <- 1 to shortcycle) {
        ObjMln.reset
        System.gc
        bpTest(workers, "C", f1)
      }
      for (test <- 1 to shortcycle) {
        ObjMln.reset
        System.gc
        bpTestScoring(workers, "CS", f1)
      }
    }
    println("Done with evaluation C")
    
    //////////////////////////////////////////////
    // D
    //////////////////////////////////////////////
    for (workers <- 1 to numberOfProcessors) {
      implicit def string2predicate(x: String) = new Predicate(x)
      val p = "http://www.semanticweb.org/ontologies/2010/4/testingowl2.owl#"
      val f1:Formula = (p+"FriendOf")~(x,y) -> ((p+"Smokes")~(x) -> (p+"Smokes")~(y)) w 0.8
      for (test <- 1 to shortcycle) {
        ObjMln.reset
        System.gc
        bpTest(workers, "D", f1)
      }
      for (test <- 1 to shortcycle) {
        ObjMln.reset
        System.gc
        bpTestScoring(workers,"DS", f1)
      }
    }
    println("Done with evaluation D")

    //////////////////////////////////////////////
    // E
    //////////////////////////////////////////////
    
    for (workers <- 1 to numberOfProcessors) {
      implicit def string2predicate(x: String) = new Predicate(x)
      val p = "http://www.semanticweb.org/ontologies/2010/4/testingowl2.owl#"
      val f1:Formula = (p+"FriendOf")~(x,y) -> ((p+"Smokes")~(x) -> (p+"HasCancer")~(y)) w 0.8
      for (test <- 1 to shortcycle) {
        ObjMln.reset
        System.gc
        bpTest(workers, "E", f1)
      }
      for (test <- 1 to shortcycle) {
        ObjMln.reset
        System.gc
        bpTestScoring(workers, "ES", f1)
      }
    }
    println("Done with evaluation E")
    
    //////////////////////////////////////////////
    // F
    //////////////////////////////////////////////
    for (workers <- 1 to numberOfProcessors) {
      implicit def string2predicate(x: String) = new Predicate(x)
    val p = "http://www.semanticweb.org/ontologies/2010/4/testingowl2.owl#"
      val f1:Formula = ((p+"Smokes")~(x) ^ (p+"Smokes")~(y)) -> (p+"HasCancer")~(z) w 0.8
      for (test <- 1 to shortcycle) {
        ObjMln.reset
        System.gc
        bpTest(workers, "F", f1)
      }
      for (test <- 1 to shortcycle) {
        ObjMln.reset
        System.gc
        bpTestScoring(workers, "FS", f1)
      }
    }
    println("Done with evaluation F")
    println("Benchmark test completed")
    //////////////////////////////////////////////
    
  }
  
 /*
  * This method defines the non-scoring belief propagaton test for evaluation.
  * It runs belief propagation without scoring on the defined Markov Logic 
  * network with multiple threads depending on the input parameter workers.
  * 
  * The test measures the times for retrieving the data from the SPARQL endpoint
  * and the grounding, the time for loading the Signal/Collect edges, the time
  * for the execution of belief propagation and finally the total run time of
  * the test. These values are then written to the evaluation.log file in the
  * /logs directory.
  * 
  * @param workers  integer value that defines number of worker threads
  * @param testId   string value that defines the test name for output only
  * @param f1       formula that defines the Markov logic formula to be grounded
  */
  def bpTest(workers: Int, testId: String, f1: Formula) = {
    
    val formulas = List(f1)
    
    val filename = "evaluation.log"
    
    // initializaton of the Markov network
    val startingTime = System.nanoTime
    val sc = ComputeGraph[Formula with Grounding](workers)
    formulas.foreach(ObjMln.addFormula(_))
    ObjMln.init(db)
    
    val meanTime = System.nanoTime

    val variableEdges = new MarkovVariableEdges(ObjMln)
    val factorEdges = new MarkovFactorEdges(ObjMln)

    // Here the graph modification feature of Signal/Collect is used in order
    // to load the data to the graph while already starting execution. This
    // reduces loading time significantly.
    val variableGraphModifications: Traversable[GraphModification[Formula with Grounding]] = new BeliefPropagationVariable(ObjMln).createEdgesWithImplicitSourceVertices(variableEdges)
    val factorGraphModifications: Traversable[GraphModification[Formula with Grounding]] = new BeliefPropagationFactor(ObjMln).createEdgesWithImplicitSourceVertices(factorEdges)

    sc.modify(variableGraphModifications)
    sc.modify(factorGraphModifications)
    
    // algorithm execution
    val bpstartingtime = System.nanoTime
    sc.execute()
    val endTime = System.nanoTime
    
    // time calculations
    val groundingTime = (meanTime - startingTime)/1000000
    val loadingTime = (bpstartingtime - meanTime)/1000000
    val executionTime = (endTime - bpstartingtime)/1000000
    val totalTime = (endTime - startingTime)/1000000
    
    // log entry to file and console
    implicit def file2helper(file: File) = new FileHelper(file)  
    val dir = new File("logs/")  
    val file = new File(dir, filename)
    val result = testId + "; " + workers + "; " + ObjMln.predicateGroundings.size + "; " + ObjMln.groundedFormulas.size + "; " + groundingTime + "; " + loadingTime + "; " + executionTime + "; " + totalTime + "\n"
    file.writeAppend(result)
    print(result)
  }
  
  /*
  * This method defines the scoring loopy belief propagaton test for evaluation.
  * It runs belief propagation with scoring on the defined Markov Logic 
  * network with multiple threads depending on the input parameter workers.
  * 
  * The test measures the times for retrieving the data from the SPARQL endpoint
  * and the grounding, the time for loading the Signal/Collect edges, the time
  * for the execution of belief propagation and finally the total run time of
  * the test. These values are then written to the evaluation.log file in the
  * /logs directory.
  * 
  * @param workers  integer value that defines number of worker threads
  * @param testId   string value that defines the test name for output only
  * @param f1       formula that defines the Markov logic formula to be grounded
  */
  def bpTestScoring(workers: Int, testId: String, f1: Formula) = {
    
   
    val formulas = List(f1)
    
    val filename = "evaluation.log"
    
    // initializaton of the Markov network
    val startingTime = System.nanoTime
    val sc = ComputeGraph[Formula with Grounding](workers)
    formulas.foreach(ObjMln.addFormula(_))
    ObjMln.init(db)
    
    val meanTime = System.nanoTime

    val variableEdges = new MarkovVariableEdges(ObjMln)
    val factorEdges = new MarkovFactorEdges(ObjMln)

    // Here the graph modification feature of Signal/Collect is used in order
    // to load the data to the graph while already starting execution. This
    // reduces loading time significantly.
    val variableGraphModifications: Traversable[GraphModification[Formula with Grounding]] = new BeliefPropagationVariableScoring(ObjMln).createEdgesWithImplicitSourceVertices(variableEdges)
    val factorGraphModifications: Traversable[GraphModification[Formula with Grounding]] = new BeliefPropagationFactorScoring(ObjMln).createEdgesWithImplicitSourceVertices(factorEdges)

    sc.modify(variableGraphModifications)
    sc.modify(factorGraphModifications)
    
    // algorithm execution
    val bpstartingtime = System.nanoTime
    sc.execute()
    val endTime = System.nanoTime
    
    // time calculations
    val groundingTime = (meanTime - startingTime)/1000000
    val loadingTime = (bpstartingtime - meanTime)/1000000
    val executionTime = (endTime - bpstartingtime)/1000000
    val totalTime = (endTime - startingTime)/1000000
    
    // log entry to file and console
    implicit def file2helper(file: File) = new FileHelper(file)  
    val dir = new File("logs/")  
    val file = new File(dir, filename)
    val result = testId + "; " + workers + "; " + ObjMln.predicateGroundings.size + "; " + ObjMln.groundedFormulas.size + "; " + groundingTime + "; " + loadingTime + "; " + executionTime + "; " + totalTime + "\n"
    file.writeAppend(result)
    print(result)
  }

}
