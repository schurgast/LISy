/*
 * @author Stefan Schurgast
 */

package ch.uzh.ifi.ddis.markovlogicinference

import scala.collection.mutable.LinkedHashMap

/*
 * open issues:
 * ------------
 *
 * TODO: Instead of how this is implemented in the following, type classes could 
 * be used, resulting in something like this:
      
      class CanMultiply[Factor]
      class CanMultiply[BeliefFactor]
      class Factor extends CanMultiply[Factor]
      class MI extends CanMultiply[MI]
*/

/*
 * The object MultiplicativeIdentity extends a BeliefFactor being the
 * multiplicative identity. Multiplying it with some factor results in the
 * factor. Multiplying it with itself results in the MultiplicativeIdentity
 * again.
 */
object MultiplicativeIdentity extends BeliefFactor

/*
 * The Factor is the general case of every factor type liek TruthFactor or
 * BeliefFactor. It extends the LinkedHashMap using a key to preserve backwards
 * traceability of the key-value pair for each value.
 */
class Factor[Key, KeyValue, Value] extends LinkedHashMap[LinkedHashMap[Key, KeyValue], Value]

/*
 * The TruthFactor extends the general Factor by using boolean values. It 
 * represents a truth table. In order to simplify the evaluation of TruthFactors,
 * the logical operators are directly implemented in the TruthFactor itself, 
 * enabling something like TruthFactor && TruthFactor.
 */
class TruthFactor extends Factor[GroundedPredicate, Boolean, Option[Boolean]] {
  
  /*
   * Logical AND for a TruthFactor. It takes two TruthFactors (this and that),
   * and combines them with a logical AND. The resulting TruthFactor has a
   * joint key of this and that as well as the resulting truth value.
   * 
   * @param that    represents right side of the logical and operation
   * 
   * @result new truth factor with joint values resulting from logical operation
   */
  def &&(that: TruthFactor): TruthFactor = {
    var result = new TruthFactor()
    
    //println("truth factor this: " + this + " that:" + that)
    
    for (thisKey <- this.keys) {
      for (thatKey <- that.keys) {
        
        //println("keys: " + thisKey + " and " + thatKey)
        
        // keys are joint
        val thisValue = this(thisKey)
        val thatValue = that(thatKey)
        val jointKey: LinkedHashMap[GroundedPredicate, Boolean] = thisKey.++(thatKey.toTraversable)
        
        // values are joint
        thisValue match {
          case Some(a) => {
            thatValue match {
              case Some(b) => result.put(jointKey, Some(a&&b)) // and if values on both sides
              case None => result.put(jointKey, None) // none if values only on one side
            }          
          }
          case None => result.put(jointKey, None) // none if no values on either side
        }
      }
    }
    result
  }
  
  /*
   * Logical OR for a TruthFactor. It takes two TruthFactors (this and that),
   * and combines them with a logical OR. The resulting TruthFactor has a
   * joint key of this and that as well as the resulting truth value.
   * 
   * @param that    represents right side of the logical OR operation
   * 
   * @result new truth factor with joint values resulting from logical operation
   */
  def ||(that: TruthFactor): TruthFactor = {
    var result = new TruthFactor()
    
    for (thisKey <- this.keys) {
      for (thatKey <- that.keys) {
        
        // joins keys
        val thisValue = this(thisKey)
        val thatValue = that(thatKey)
        val jointKey: LinkedHashMap[GroundedPredicate, Boolean] = thisKey.++(thatKey.toTraversable)
        
        // joins values
        thisValue match {
          case Some(a) => {
            thatValue match {
              case Some(b) => result.put(jointKey, Some(a||b)) // logical or if both sides have values
              case None => result.put(jointKey, Some(a)) // only a if only a has values
            }          
          }
          case None => {
            thatValue match {
              case Some(b) => result.put(jointKey, Some(b)) // only b if only b has values
              case None => result.put(jointKey, None) // none if no values on both sides
            }
          }
        }
      }
    }
    result
  }
  
  /*
   * Logical IMPLIES for a TruthFactor. Since the implies-operator for a given
   * formula a -> b can be expressed as not(a) || b, the operation is replaced.
   * (Recall that from something wrong, anything can follow (Modus ponens))
   * 
   * IMPLIES takes two TruthFactors (this and that) and combines them with a 
   * logical statement as given above. The resulting TruthFactor has a
   * joint key of this and that as well as the resulting truth value.
   * 
   * @param that    represents right side of the logical IMPLIES operation
   * 
   * @result new truth factor with joint values resulting from logical operation
   */
  def ->(that: TruthFactor): TruthFactor = {
    var result = new TruthFactor()
    
    for (thisKey <- this.keys) {
      val thisValue = this.get(thisKey).get
      for (thatKey <- that.keys) {
        
        // join keys
        val thatValue = that.get(thatKey).get
 //       println("thisvalue: " +thisValue)
 //       println("thatvalue: " +thatValue)
        val jointKey: LinkedHashMap[GroundedPredicate, Boolean] = thisKey.++(thatKey.toTraversable)
        
        // join values
        thisValue match {
          case Some(a) => {
            thatValue match {
              case Some(b) => result.put(jointKey, Some(!a||b)) // logical implies if both sides have values
              case None => result.put(jointKey, Some(!a)) // not a, if only left side has values
            }          
          }
          case None => {
            thatValue match {
              case Some(b) => result.put(jointKey, Some(b)) // b, if only right side has values
              case None => result.put(jointKey, None) // if no side has values, return none
            }
          }
        }
 //     println("tmp result: " + result)
      }
    }
    //println("-> result is: "+ result)
    result
  }
  
  /*
   * Logical EQUALITY for a TruthFactor. EQUALITY takes two TruthFactors 
   * (this and that) and combines them with a logical statement as given above. 
   * The resulting TruthFactor has a joint key of this and that as well as the 
   * resulting truth value.
   * 
   * @param that    represents right side of the logical EQUALITY operation
   * 
   * @result new truth factor with joint values resulting from logical operation
   */
  def <->(that: TruthFactor): TruthFactor = {
    var result = new TruthFactor()
    
    for (thisKey <- this.keys) {
      val thisValue = this.get(thisKey).get
      for (thatKey <- that.keys) {
        
        val thatValue = that(thatKey)
        val jointKey: LinkedHashMap[GroundedPredicate, Boolean] = thisKey.++(thatKey.toTraversable)
        
        thisValue match {
          case Some(a) => {
            thatValue match {
              case Some(b) => result.put(jointKey, Some(a==b)) // use operator, if both sides have values
              case None => result.put(jointKey, None) // for only one value, equality does not make sense
            }          
          }
          case None => result.put(jointKey, None) // return none for equality on nothing
        }
      }
    }
    result
  }
  
  /*
   * Logical NOT for an unary TruthFactor. NOT takes one TruthFactors 
   * (that while ignoring this) and logically inverses the value.
   * The resulting TruthFactor has the same key of that as well as the 
   * resulting truth value.
   * 
   * @param that    represents right side of the logical NOT operation
   * 
   * @result new truth factor with inverted value
   */
  def !(that: TruthFactor): TruthFactor = {
    var result = new TruthFactor()
    for (thatKey <- that.keys) {
      val thatValue = that(thatKey) match {
        case Some(a) => Some(!a)
        case None => None
        case _ => throw new Exception(this + ": not supposed to happen.")
      }
      result += ((thatKey, thatValue))
    }
    result
  }
}

/*
 * The object TruthFactor is the companion object of the class TruthFactor.
 */
object TruthFactor {
  
  /*
   * The apply function returns a new TruthFactor as initialized with the map.
   * 
   * @param map   initialization map for the truth factor
   * 
   * @return new truth factor initialized with the map
   */
  def apply(map: LinkedHashMap[LinkedHashMap[GroundedPredicate, Boolean],Option[Boolean]]): TruthFactor = {
    var truthFactor = new TruthFactor()
    for (i <- map) { 
      truthFactor += i
    }
    truthFactor
  }
}

/*
 * The BeliefFactor extends the general Factor by using double values. It 
 * represents a probability distribution. In order to simplify the joining of 
 * BeleifFactors (i.e. to receive the joint probability distribution), the join
 * operation is directly implemented in the TruthFactor itself, as well as the
 * marginalize and the normalize operation.
 */
class BeliefFactor extends Factor[GroundedPredicate, Boolean, Option[Double]] {

  var sum: Double = 0

  /*
   * The join operations returns the joint probability distribution of this and
   * that, also with respect to the multiplicative identity.
   * 
   * @param that    BeliefFactor for right side of the join operation
   * 
   * @return the joint probability distribution of type BeliefFactor
   */
  def join(that: BeliefFactor): BeliefFactor = {
    var result:BeliefFactor = new BeliefFactor()
    
    // handle special case for multiplicative identity
    if (this.equals(MultiplicativeIdentity)) {
      that
    } else if (that.equals(MultiplicativeIdentity)) {
      this
    } else {
      sum = 0
      for (thisKey <- this.keys) {
        
        val thisValue = this(thisKey)
        for (thatKey <- that.keys) {
          
          // join keys
          val thatValue = that(thatKey)
          val jointKey: LinkedHashMap[GroundedPredicate, Boolean] = thisKey.++(thatKey.toTraversable)

          // join values
          thisValue match {
            case Some(a) => {
              thatValue match {
                case Some(b) => val ab = a*b; result.put(jointKey, Some(ab)); sum += ab // only accept operation with values on both sides of the operator
                case None => result.put(jointKey, None)
              }          
            }
            case None => result.put(jointKey, None)
          }
        }
      }
    }
    result
  }

  /*
   * The multiplication returns the product of the two BeliefFactors this
   * and that, also with respect to the multiplicative identity.
   * 
   * @param that    is the right side of the operation
   * 
   * @return product of the left and right side of the operation
   */
  def *(that: BeliefFactor): BeliefFactor = {
    var result:BeliefFactor = new BeliefFactor()
    //println("this: "+this)
    //println("that: "+that)
    if (this.equals(MultiplicativeIdentity)) {
      that
    } else if (that.equals(MultiplicativeIdentity)) {
      this
    } else {
      sum = 0
      for (key <- keys) {
        var newValue: Option[Double] = None
        get(key).get match {
          case Some(a) => {
              
              try {
              
                if (that.contains(key)) {
                  that.get(key).get match {
                    case Some(b) => newValue = Some(get(key).get.get * that.get(key).get.get)
                    case None => newValue = None
                  }
                } else {
                  // TODO: There seems to be a bug somewhere around here or in the thread handling of Signal/Collect. Most of the time the tests run through without crashing. But sometimes this happens out of a sudden with the same parameters and the same data given. Not really intuitive.
                  throw new Exception("BeliefFactor multiplication failed. That does not contain " + key + ". That is " + that + ".")

                }
              
              } catch {
                
                case e: Exception => {
                    System.err.println("Multiplication of BeliefFactor failed, but error catched. Keep looking for the bug. That does not contain " + key + ". That is " + that + ".")
                    newValue = None // TODO: not sure if this is a valid catch
                }
              }
          }
          case None => newValue = None
        }
        result.put(key, newValue)
        sum += newValue.getOrElse(0.0)
      }
      //println("tmpresult multiplication: " + result)
      result
    }
  }
  
  
  /*
   * The addition returns the sum of the two BeliefFactors this
   * and that, also with respect to the multiplicative/additive identity.
   * 
   * @param that    is the right side of the operation
   * 
   * @return sum of the left and right side of the operation
   */
  def +(that: BeliefFactor): BeliefFactor = {
    var result:BeliefFactor = new BeliefFactor()
    if (this.equals(MultiplicativeIdentity)) {
      that
    } else if (that.equals(MultiplicativeIdentity)) {
      this
    } else {
      sum = 0
      for (key <- keys) {
        var newValue: Option[Double] = None
        this.get(key).get match {
          case Some(a) => {
              if (that.contains(key)) {
                that.get(key).get match {
                  case Some(b) => newValue = Some(this.get(key).get.get + that.get(key).get.get)
                  case None => newValue = Some(this.get(key).get.get)
                  case _ => throw new Exception("This does not match anything.")
                }
              } else {
                throw new Exception("BeliefFactor addition failed. That does not contain " + key)
              }
          }
          case None => newValue = None
        }
        result.put(key, newValue)
        sum += newValue.getOrElse(0.0)
      }
      result
    }
  }

  /*
   * The inversion method returns the inverted probability (meaning 1-p) for 
   * that, ignoring this as it is an unary operation.
   * 
   * @param that    probability distribution to be inverted
   * 
   * @result inverted probability distribution
   */
  def !(that: BeliefFactor): BeliefFactor = {
    var result: BeliefFactor = new BeliefFactor()
    sum = 0.0
    for (key <- that.keys) {
      val truthValue = that(key) match {
        case Some(a) => Some(1-a)
        case None => None
      }
      result.put(key,truthValue)
      sum += truthValue.getOrElse(0.0)
    }    
    result
  }

  /*
   * Returns the marginalized probability distribution for a given grounded
   * predicate.
   * 
   * @param allExceptGP   grounded predicate that marginalization is for
   * 
   * @return  marginalized probability distribution
   */
  def marginalize(allExceptGP: GroundedPredicate): BeliefFactor = {
    var result = new BeliefFactor()

    var trueSum = 0.0
    var falseSum = 0.0
    for(rowKey <- this.keys) {
      if (rowKey.contains(allExceptGP)) {
        val truth = rowKey.get(allExceptGP).get
        val configurationValue = this.get(rowKey).get match {
            case Some(value) => value
            case _ => 0.0
          }
        if (truth) {
          trueSum += configurationValue
        } else {
          falseSum += configurationValue
        }
      }
    }
    sum = trueSum + falseSum

    result.put(LinkedHashMap(allExceptGP -> true),Some(trueSum))
    result.put(LinkedHashMap(allExceptGP -> false),Some(falseSum))
    result
  }

  
  /*
   * Operaton normalizes the values of the of a probability distribution, and 
   * updates the Factor. Negative values should never occure.
   */
  def normalize {
    if (values.size > 0) {
      val sum = values.foldLeft(0.0){
        (aggr,value) => {
          value match {
            case Some(d) => aggr + d
            case _ => aggr
          }
        }
      }
      if (sum > 0) {
        keys.foreach{ k =>
          get(k).get match {
            case Some(d) => put(k, Some(d/sum))
            case None =>
          }
        }
      } else { // TODO: contract: no negative values in map
        keys.foreach{ k =>
          get(k).get match {
            case Some(d) => put(k, Some(1/values.size))
            case None =>
          }
        }
      }
      this.sum = 1
    }
    this.sum = 0
  }
}

/*
 * The object BeliefFactor is the companion object of the class BeliefFactor.
 */
object BeliefFactor {
  
  /*
   * The apply function returns a new BeliefFactor as initialized with the map.
   * 
   * @param map   initialization map for the belief factor
   * 
   * @return new belief factor initialized with the map
   */
  def apply(map: LinkedHashMap[LinkedHashMap[GroundedPredicate, Boolean],Option[Double]]): BeliefFactor = {
    var beliefFactor = new BeliefFactor()
    for (i <- map) { 
      beliefFactor += i
    }
    beliefFactor
  }
}