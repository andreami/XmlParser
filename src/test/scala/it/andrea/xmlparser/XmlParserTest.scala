package it.andrea.xmlparser

import org.scalatest.FunSuite
import it.andrea.xmlparser.XmlParser._

class XmlParserTest extends FunSuite {
  test("example") {
    case class Sample(a: Int, b: Boolean, c: Double, d: String)
    println(generate[Sample])
  }
}