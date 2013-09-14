/*
 * @author Stefan Schurgast
 */

package ch.uzh.ifi.ddis.markovlogicinference

/**
* Class MarkovFactorEdges represents a edge between the factor node and the
* variable node in the markov network graph. With Markov Logic, factors are
* represented by grounded formulas, and variables are represented by grounded
* predicates.
*
* @param  mln     markov logic network on which belief propagation is performed
*/
class MarkovFactorEdges(
  val mln: MarkovLogicNetwork
) extends Traversable[(Formula with Grounding, Formula with Grounding)]
{
  /* source nodes are factors representated by grounded formulas in MLN */
  val sourceNodes = mln.groundedFormulas

  /**
  * Returns a tuple for every created edge composed of source and target node
  *
  * @return markov factor edge as tuple of source and target
  */
  def foreach[U](f: ((Formula with Grounding, Formula with Grounding)) => U) =
  {
    for (source <- sourceNodes) {
      for (target <- source.groundedPredicates) {
        f((source, target))
      }
    }
  }
}

/**
* Class MarkovVariableEdges represents a edge between the variable node and the
* factor node in the markov network graph. With Markov Logic variables are
* represented by grounded predicates and factors are represented by grounded
* formulas.
*
* @param  mln     markov logic network on which belief propagation is performed
*/
class MarkovVariableEdges(
  val mln: MarkovLogicNetwork
) extends Traversable[(Formula with Grounding, Formula with Grounding)]
{

  /* source nodes are variables representated by grounded predicates in MLN */
  val sourceNodes = mln.predicateGroundings

  /**
  * Returns a tuple for every created variable edge composed of source and
  * target node.
  *
  * @return markov variable edge as tuple of source and target
  */
  def foreach[U](f: ((Formula with Grounding, Formula with Grounding)) => U) = {
    for (source <- sourceNodes) {
      for (target <- source.rootFormulas) {
        target match {
          case gp: GroundedPredicate => throw new Exception("ERROR: target is a grounded predicate " + gp)
          case _ => {
            f((source, target))
          }
        }
      }
    }
  }
}