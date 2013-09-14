/*
 *  @author Philip Stutz
 */

package ch.uzh.ifi.ddis.markovlogicinference

import ch.uzh.ifi.ddis.signalcollect._
import java.io.BufferedReader
import java.io.InputStreamReader
import org.apache.log4j.ConsoleAppender
import org.apache.log4j.FileAppender
import org.apache.log4j.Level
import org.apache.log4j.SimpleLayout
import org.junit._
import Assert._

class FormulaTest { //extends Markov { //TODO: wo scope genau mit variablen, etc.

  val layout = new SimpleLayout
    Logging.addAppender(new ConsoleAppender(layout))
    Logging.addAppender(new FileAppender(layout, "logs/measurements.log", false))
    Logging.setLevel(Level.ALL)

  
def not(f: Formula) = new Not(f)
def ¬(f:Formula) = new Not(f)
implicit def string2predicate(x: String) = new Predicate(x)
      
  
def profilerHook = {
    println("Connect the profiler and press any key to continue:")
    val in = new InputStreamReader(System.in)
    val reader = new BufferedReader(in)
    reader.readLine
    println("Starting ...")
  }

    @Before
    def setUp: Unit = {
    }

    @After
    def tearDown: Unit = {
    }

@Test 
    def groundMarkovNetwork = {
      
      val db = SesameSparql("http://athena.ifi.uzh.ch:8080/openrdf-sesame", "tinysmokes")
      val sc = new ComputeGraph(1)

      val p = "http://www.semanticweb.org/ontologies/2010/4/testingowl2.owl#"
      val f1 = (p+"Smokes")~(x) -> (p+"HasCancer")~(y) w 100
      val formulas = List(f1)

      formulas.foreach(ObjMln.addFormula(_))
      
      ObjMln.init(db)
    
      ObjMln.printGroundedPredicates
      ObjMln.printGroundedFormulas
    
      sc.add(new BeliefPropagationVariable(ObjMln))
      sc.add(new BeliefPropagationFactor(ObjMln))

      sc.execute
    }
}