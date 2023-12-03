package utils

import scala.io.Source

object inputReader {
  private val currentDirectory = new java.io.File(".").getCanonicalPath

  def lines(fileName: String): List[String] = Source
    .fromFile(
      s"$currentDirectory/src/main/resources/$fileName"
    )
    .getLines
    .toList

  def wholeFile(fileName: String): String =
    Source.fromFile(s"$currentDirectory/src/main/resources/$fileName").mkString
}
