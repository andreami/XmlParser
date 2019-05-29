package it.andrea.xml

import java.io.StringWriter
import java.util.Properties

import com.codecommit.antixml.{Elem, Node, StAXParser, _}
import javax.xml.parsers.DocumentBuilder
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.{OutputKeys, TransformerFactory}
import org.w3c.dom
import org.w3c.dom.Element

object XmlOps {

  implicit def toAntiXml(elem: scala.xml.Elem): Elem = elem.convert

  val docBuilder: DocumentBuilder = javax.xml.parsers.DocumentBuilderFactory.newInstance().newDocumentBuilder()
  val parser = new StAXParser()

  implicit class JavaDocumentOps(val inner: org.w3c.dom.Document) extends AnyVal {
    def toAntiXml: Elem = parser.fromString(XmlToString.toString(inner))
  }

  implicit def nodeExtras(n: Node): NodeExtras = new NodeExtras(n)

  implicit def elemExtras(e: Elem): ElemExtras = new ElemExtras(e)
}

class NodeExtras(n: Node) {

  import it.andrea.xml.XmlOps._

  def toJdkNode(doc: org.w3c.dom.Document): org.w3c.dom.Node =
    n match {
      case Elem(_, label, attributes, _, children) =>
        val r = doc.createElement(label)
        for (a <- attributes) {
          r.setAttribute(a._1.name, a._2)
        }
        for (c <- children) {
          r.appendChild(c.toJdkNode(doc))
        }
        r
      case _ => doc
    }
}

class ElemExtras(e: Elem) extends NodeExtras(e) {
  override def toJdkNode(doc: org.w3c.dom.Document): Element =
    super.toJdkNode(doc).asInstanceOf[org.w3c.dom.Element]

  def toW3cDocument: dom.Document = {
    val doc = XmlOps.docBuilder.newDocument()
    doc.appendChild(toJdkNode(doc))
    doc
  }
}

object XmlToString {
  val tidy: Properties = {
    val p = new Properties()
    p.setProperty(OutputKeys.STANDALONE, "yes")
    p.setProperty(OutputKeys.METHOD, "xml")
    p.setProperty(OutputKeys.ENCODING, "UTF-8")
    p.setProperty(OutputKeys.INDENT, "no")
    p.setProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    p
  }

  def toString(element: org.w3c.dom.Node): String = {
    val domSource = new DOMSource(element)
    val writer = new StringWriter()
    val streamResult = new StreamResult(writer)
    val tf = TransformerFactory.newInstance()
    val transformer = tf.newTransformer()
    transformer.setOutputProperties(tidy)
    transformer.transform(domSource, streamResult)
    writer.toString
  }

}
