package net.crinklejoint

import java.awt.Shape
import java.awt.geom.AffineTransform
import org.w3c.dom
import org.w3c.dom.NodeList
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.parsers.DocumentBuilder
import javax.xml.xpath.XPathFactory
import javax.xml.xpath.XPathConstants


object SVGParser {


  def readFile(file: String) = {
    val factory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance
    val builder: DocumentBuilder = factory.newDocumentBuilder
    val document: dom.Document = builder.parse(file)
    val xpathExpression = "//path/@d"
    val xpf = XPathFactory.newInstance
    val xpath = xpf.newXPath
    val expression = xpath.compile(xpathExpression)
    val svgPaths = expression.evaluate(document, XPathConstants.NODESET).asInstanceOf[NodeList]


    val node = svgPaths.item(0).getNodeValue.toString


    print(s">>> $node")
    parsePathShape(node)
    "string"

  }

  import org.apache.batik.parser.AWTPathProducer
  import org.apache.batik.parser.PathParser
//  import java.awt.geom.Rectangle2D

  def parsePathShape(svgPathShape: String) = try {
    val pathProducer = new AWTPathProducer
    val pathParser = new PathParser
    pathParser.setPathHandler(pathProducer)
    pathParser.parse(svgPathShape)
    val shape = pathProducer.getShape
    val iterator = shape.getPathIterator(null)
    var coords: Array[Float] = new Array[Float](6)
    while(!iterator.isDone) {

      iterator.currentSegment(coords)
      iterator.next()

      println(s">> ${(coords(0), coords(1))}")
      "next"
    }

    "did i parse?"
//    pathProducer.getShape
  } catch {
    case ex => println(s"EXCEPT $ex")
// Fallback to default square shape if shape is incorrect
//      new Rectangle2D.Float(0, 0, 1, 1)
  }

}