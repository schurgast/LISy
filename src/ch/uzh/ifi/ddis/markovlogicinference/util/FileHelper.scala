/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ch.uzh.ifi.ddis.markovlogicinference

import java.io._

class FileHelper(file: File) {
  def write(text: String) = {
    val fileWriter = new FileWriter(file)
    try{
      fileWriter.write(text)
    } finally {
      fileWriter.close
    }
  }
  def writeAppend(text: String) = {
    val fileWriter = new FileWriter(file, true)
    try{
      fileWriter.write(text)
    } finally {
      fileWriter.close
    }
  }
  
  def foreachLine(proc: String => Unit) = {
    val bufferedReader = new BufferedReader(new FileReader(file))
    try{
      while(bufferedReader.ready) proc(bufferedReader.readLine)
    } finally {
      bufferedReader.close
    }
  }
}

object FileHelper {
  implicit def file2helper(file: File) = new FileHelper(file)
}