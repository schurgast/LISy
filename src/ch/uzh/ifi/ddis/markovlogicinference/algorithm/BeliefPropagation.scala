/*
 * @author Stefan Schurgast
 */

package ch.uzh.ifi.ddis.markovlogicinference

import ch.uzh.ifi.ddis.signalcollect.ComputeSubgraph
import java.text.NumberFormat
import scala.collection.mutable.LinkedHashMap


/**
* Class BeliefPropagationVariable represents the variable node for belief
* propagation in a Markov Logic Network. This class extends the ComputeSubgraph
* of the SignalCollect framework.
*
* TODO: custom class for id that also defines "val hashCode = whatever.hashCode"
*
* @param  mln  markov logic network used for reasoning algorithm
*/
class BeliefPropagationVariable(
  val mln: MarkovLogicNetwork
) extends ComputeSubgraph[Formula with Grounding]
{
  /* IdType of variable node is a Formula with Grounding. */
  type IdType = Formula with Grounding

  /* StateType of variable node is probability distribution of type BeliefFactor */
  type StateType = BeliefFactor

  /* Messages are probability distributions of type BeliefFactor. */
  type IncomingSignalType = BeliefFactor
  type OutgoingSignalType = BeliefFactor

  /**
  * Variable node has MarkovVariableEdges. This method is not active anymore
  * because the edges are now loaded with the graph modification feature 
  * already to reduce loading times.
  *
  * @return markov edges for markov logic network
  */
  //def edges = new MarkovVariableEdges(mln)

  /**
  * Returns the multiplicative identity factor for the initial state of the
  * vertex.
  *
  * @return initial state of variable node of type BeliefFactor
  */
  override def initialVertexState(vertexId: IdType): BeliefFactor = {
    MultiplicativeIdentity
    
    // setting evidence in intitial state does not make belief propagation faster
    /*
    val thisVertex: GroundedPredicate = vertexId.asInstanceOf[GroundedPredicate]
    var result: BeliefFactor = new BeliefFactor()
    vertexId.evidence match {
      case TRUE => {
        result.put(LinkedHashMap(thisVertex -> true),Some(1.0))
        result.put(LinkedHashMap(thisVertex -> false),None)
      }
      case FALSE => {
        result.put(LinkedHashMap(thisVertex -> true),None)
        result.put(LinkedHashMap(thisVertex -> false),Some(1.0))
      }
      case _ => MultiplicativeIdentity
    }
    result
    */
  }
  
  /**
  * Signals are sent from this variable node to factor nodes. The message from
  * each variable to its connected factors before normalizing is:
  *
  * message(x->f)(x) = prod(element h of set factor x appears in without f, message(h->x)(x)
  *
  * @param  e   edge over which signal is sent
  * @return signal that is sent from variable node to factor node
  */
  def signal(e: Edge): BeliefFactor = {
    //val state: BeliefFactor = e.source.state
    //println("variable source state: " +state + " on edge: " + e)
    
    //Maybe the signal creation could be speed up by deviding by the target
    //contribution instead of checking the target for every single incoming 
    //signal. This has not been tested yet. To get target contribution use:
    //val targetContribution: Option[BeliefFactor] = e.source.signals.get(e.targetId)

    val MI: BeliefFactor = MultiplicativeIdentity
    var signal: BeliefFactor = e.source.signals.foldLeft(MI){
      (aggr: BeliefFactor, msg:(Formula with Grounding, BeliefFactor)) => {
        if (msg._1.equals(e.targetId)) 
          aggr
        else {
          //println("msg: " + msg._2)
          aggr*msg._2
        }
      }
    }
    //println("signal from variable to factor: "+signal)
    signal.normalize // TODO: simplify expensive method normalize
    signal
  }

  /**
  * Collects signal of vertex and aggregates by multiplying all messages in 
  * message queue.
  *
  * @param  v   vertex that collects signals
  * @return normalized state of source node as joint probability distribution
  */
  def collect(v: Vertex): BeliefFactor = {

    val MI: BeliefFactor = MultiplicativeIdentity
    var state: BeliefFactor = v.signals.values.foldLeft(MI){
      (aggr: BeliefFactor, msg:BeliefFactor) => aggr*msg
    } // original solution
    
    /*
    state = v.signals.values.foldLeft(MI){
      (aggr: BeliefFactor, msg:BeliefFactor) => aggr*msg
    } */ // incorporate evidence in new solution
    
    //println("variable: " + v.id + " " + v.state)
    state
    // TODO: divide by target contribution
  }
  
  /**
  * Returns result of vertex state after running belief propagation. The state
  * is first normalized and then, for better readability, reformatted.
  *
  * @param  v   vertex that collects signals
  * @return result of reasoning algorithm
  */
  override def processResult(v: Vertex) = {
    val formatter = NumberFormat.getInstance()
    //println("unnormalized result: " + v.id + ": " + v.state)
    v.state.normalize
    println("result: " + v.id + ": " + formatter.format(v.state.get(LinkedHashMap(v.id.asInstanceOf[GroundedPredicate] -> true)).get.get))
  } 
}
/*
 * This class extends the class BeliefPropagationVariable with scoring 
 * capabilities. It therefore implements the methods scoreCollect and 
 * scoreSignal to achieve a faster convergence.
 * 
 * @param mln   Markov Logic Network
 */
class BeliefPropagationVariableScoring(
  mln: MarkovLogicNetwork
) extends BeliefPropagationVariable(mln: MarkovLogicNetwork)
{

  /*
   * This method scoreCollect returns the number of uncollected messages. The
   * higher the number of messages, the more important the node must be, and 
   * therefore, the higher the score in order to execute as fast as possible.
   * 
   * @param v   vertex to be scored
   * 
   * @return score value of type double
   */
  override def scoreCollect(v: Vertex): Double = {
    v.uncollectedMessages.size
  }
  
  /*
   * This method scoreSignal returns a score value that is the difference
   * between the old and the new state. This assures, that vertices with a
   * higher change in state are scored higher than others.
   * 
   * @param v   vertex to score
   * 
   * @return score value of type double
   */
  override def scoreSignal(v: Vertex): Double = {
    v.lastSignalState match {
      case None => 1.0
      case Some(oldState) => math.abs(oldState.sum-v.state.sum)

// old and slow implementation of the scoreSignal
//      
//      case Some(previousState) => {
//          var diff = 0.0
//          for (key <- v.state.keys) {
//            previousState.get(key) match {
//              case Some(value) => diff += math.abs(v.state.get(key).get.get - value.get)
//              case None => diff += v.state.get(key).get.get
//            }
//          }
//          diff
//        }
    }
  }  
}


/**
* Class BeliefPropagationFactor represents the factor node for belief
* propagation in a Markov Logic Network. This class extends the class
* ComputeSubgraph of the SignalCollect framework.
*
* @param  mln  Markov logic network on which belief propagation is executed
*/
class BeliefPropagationFactor(
  val mln: MarkovLogicNetwork
) extends ComputeSubgraph[Formula with Grounding]
{
  /* IdType of factor node is a Formula with Grounding. */
  type IdType = Formula with Grounding

  /* StateType of factor node is probability distribution with truthvalue and double. */
  type StateType = BeliefFactor

  /* Messages as probability distributions of type BeliefFactor. */
  type IncomingSignalType = BeliefFactor
  type OutgoingSignalType = BeliefFactor

  /**
  * Factor node has MarkovFactorEdges. This method is not active anymore
  * because the edges are now loaded with the graph modification feature 
  * already to reduce loading times.
  * 
  * @return markov edges for markov logic network
  */
  //def edges = new MarkovFactorEdges(mln)

  /**
  * Returns the multiplicative identity factor for the initial state of the
  * vertex.
  *
  * @return initial state of factor node of type BeliefFactor
  */
  override def initialVertexState(vertexId: Formula with Grounding): BeliefFactor = {
    MultiplicativeIdentity
  }

  /**
  * Signals are sent from this factor node to variable nodes. Therefore all
  * messages from variable nodes are multiplied except the ones comming from
  * the target in order to not incorporate the target contribution. The result
  * is then multiplied by the factor (exp(weight*f)) and then marginalized by
  * the predicate before returning the signal.
  *
  * @param  e   edge over which signal is sent
  * 
  * @return state of source node as joint probability distribution
  */
  def signal(e: Edge): StateType = {
    
    //val state: BeliefFactor = e.source.state
    //val targetContribution: Option[BeliefFactor] = e.source.signals.get(e.targetId)

    val MI: BeliefFactor = MultiplicativeIdentity
    val product: BeliefFactor = e.source.signals.foldLeft(MI){
      (aggr: BeliefFactor, msg:(Formula with Grounding, BeliefFactor)) => {
        if (msg._1.equals(e.targetId)) 
          aggr
        else 
          aggr*msg._2
      }
    }
    val factor = e.source.id.getEvidenceWeightMap
    //println("factor: " + factor)
    //println("product: " + product)
    val unmarginalizedSignal = factor*product
    
    val predicateId: GroundedPredicate = e.targetId match {
      case gp: GroundedPredicate => gp
      case _ => throw new Exception("Factors should only signal to variables.")
    }

    val signal = unmarginalizedSignal.marginalize(predicateId)
    //println("signal from factor to variable: "+ signal)
    signal
  }

  /**
  * Collects signal of vertex and aggregates messages by multiplying all the
  * values. The new state is the the factor multiplied by the aggregated message
  * product.
  *
  * @param  v   vertex that collects signals
  * @return state of source node as joint probability distribution
  */
  def collect(v: Vertex): StateType = {
    val MI: BeliefFactor = MultiplicativeIdentity
    val product: BeliefFactor = v.signals.foldLeft(MI){
      (aggr: BeliefFactor, msg:(Formula with Grounding, BeliefFactor)) => {
          aggr join msg._2
      }
    }
    val factor = v.id.getEvidenceWeightMap

    val newState = factor*product
    //println("I'm vertex " + v + " with new state " + newState)
    newState
  }
  
  /**
  * Result method should not do anything, as the results are processed in the
  * variable nodes already.
  *
  * @param  v   vertex that collects signals
  * @return result of reasoning algorithm (should be nothing)
  */
  override def processResult(v: Vertex) = {
  }
}

/*
 * This class extends the class BeliefPropagationFactor with scoring 
 * capabilities. It therefore implements the methods scoreCollect and 
 * scoreSignal to achieve a faster convergence.
 * 
 * @param mln   Markov Logic Network
 */
class BeliefPropagationFactorScoring(
  mln: MarkovLogicNetwork
) extends BeliefPropagationFactor(mln: MarkovLogicNetwork)
{

  /*
   * This method scoreCollect returns the number of uncollected messages. The
   * higher the number of messages, the more important the node must be, and 
   * therefore, the higher the score in order to execute as fast as possible.
   * 
   * @param v   vertex to be scored
   * 
   * @return score value of type double
   */
  override def scoreCollect(v: Vertex): Double = {
    v.uncollectedMessages.size
  }

  /*
   * This method scoreSignal returns a score value that is the difference
   * between the old and the new state. This assures, that vertices with a
   * higher change in state are scored higher than others.
   * 
   * @param v   vertex to score
   * 
   * @return score value of type double
   */
  override def scoreSignal(v: Vertex): Double = {
    v.lastSignalState match {
      case None => 1.0
      case Some(oldState) => math.abs(oldState.sum-v.state.sum)
        
// previous scoring function that was to slow and too complex
// 
//      case Some(previousState) => {
//          var diff = 0.0
//          for (key <- v.state.keys) {
//            previousState.get(key) match {
//              case Some(value) => {
//                  value match {
//                    case Some(dblvalue) => diff += math.abs(v.state.get(key).get.get - dblvalue)
//                    case None => 0.0
//                  }
//                }
//              case None => {
//                  v.state.get(key).get match {
//                    case Some(dblvalue) => diff += dblvalue
//                    case None => 0.0
//                  }
//                }
//            }
//          }
//          diff
//        }
    }
    
  }
}