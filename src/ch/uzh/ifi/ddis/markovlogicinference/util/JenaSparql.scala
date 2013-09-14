/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ch.uzh.ifi.ddis.markovlogicinference

/*
import com.hp.hpl.jena._
import virtuoso.jena.driver._
import ch.uzh.ifi.ddis.signalcollect._


class JenaSparql(serverUrl: String, serverPort: Int) extends SparqlAccessor{
  val set: VirtGraph = new VirtGraph(serverUrl + ":" + serverPort, "dba", "dba")

  def sparqlExecute(query: String): ResultSet = {
    val sparql: Query = QueryFactory.create(query)
    val vqe: VirtuosoQueryExecution = VirtuosoQueryExecutionFactory.create (sparql, set);
    vqe.execSelect()
  }
}
*/