/*
 * @author Stefan Schurgast
 */

package ch.uzh.ifi.ddis.markovlogicinference

/**
* Abstract class TruthValue represents the truth value that can be taken by a
* grounded predicate or a grounded formula. It is either a Option for a boolean
* value Some(true) (object TRUE) or Some(false) (object FALSE), or unknown
* (object UNKNOWN where Option is None).
*
* @param  value       Option of boolean represents truth value.
*/
abstract class TruthValue(val value: Option[Boolean])

/* object for truth value Some(true) */
object TRUE extends TruthValue(Some(true)) {

  /**
  * Returns string representation of option Some(true) as defined in XML schema.
  * @return string representation of true
  */
  override val toString = "true"
  val rdfString = "\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>"
}

/* object for truth value Some(false) */
object FALSE extends TruthValue(Some(false)) {
  
  /**
  * Returns string representation of option Some(false) as defined in XML schema.
  * @return string representation of false
  */
  override val toString = "false"
  val rdfString = "\"false\"^^<http://www.w3.org/2001/XMLSchema#boolean>"
}

/* object for truth value None */
object UNKNOWN extends TruthValue(None) {

/**
  * Returns string representation of option None.
  * @return string representation of unknown
  */
  override val toString = "unknown"
  val rdfString = "unknown"
}
