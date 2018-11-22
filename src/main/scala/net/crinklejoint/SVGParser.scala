package net.crinklejoint

import java.awt.{Rectangle, Shape}
import java.awt.geom.{AffineTransform, GeneralPath, PathIterator}
import java.io.{File, FileOutputStream}
import org.w3c.dom
import org.w3c.dom.NodeList
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.parsers.DocumentBuilder
import javax.xml.xpath.XPathFactory
import javax.xml.xpath.XPathConstants
import org.apache.batik.parser.{FloatArrayProducer, PointsParser}


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


    val node = svgPaths.item(0).getNodeValue


    println(s">>> $node")
    parsePathShape(node)
    "string"
    node

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
    val iterator: PathIterator = shape.getPathIterator(null)

//    while (!iterator.isDone) {
//
//      """
//        |>> (26.458332,196.9875)
//        |>> (22.489582,193.41562)
//        |>> (26.458332,189.84375)
//        |>> (18.520832,189.84375)
//        |>> (22.489582,193.41562)
//        |>> (18.520832,196.9875)
//      """.stripMargin
//
//      iterator.currentSegment(coords)
//      println(s">> ${(coords(0), coords(1))}")
//      iterator.next()
//
//      "next"
//    }


    iterator
  }

  def parsePoints() = {
    val pointsProducer = new FloatArrayProducer
    val pointsParser = new PointsParser
    pointsParser.setPointsHandler(pointsProducer)
  }


  import org.apache.batik.dom.GenericDOMImplementation
  import org.apache.batik.svggen.SVGGraphics2D
  import org.w3c.dom.DOMImplementation
  import java.awt.Color
  import java.awt.Graphics2D
  import java.io.IOException
  import java.io.OutputStreamWriter
  import java.io.Writer

  def paint(g2d: Graphics2D): Unit = {
//    g2d.setPaint(Color.red)
    val path = new GeneralPath
    val iterator = parsePathShape("m 27.78125,107.69063 -3.96875,4.7625 -3.96875,-4.7625 0,8.33437 3.96875,-4.7625 3.96875,4.7625")
    path.append(iterator, false)

    val p2 = new GeneralPath
    val i2 = parsePathShape("m 10.583333,204.13125 h 14.552083 v 9.525 l -6.614583,-4.7625 h -7.9375 z")
    p2.append(i2, false)
    
    g2d.draw(path)
    g2d.draw(p2)
  }

  @throws[IOException]
  def writeOut(): Unit = {
    // Get a DOMImplementation.
    val domImpl = GenericDOMImplementation.getDOMImplementation
    // Create an instance of org.w3c.dom.Document.
    val svgNS = "file:///home/owenpaulmeyer/src/nnw_gds/svg_test.svg"
    val document = domImpl.createDocument(svgNS, "svg", null)
    // Create an instance of the SVG Generator.
    val svgGenerator = new SVGGraphics2D(document)
    // Ask the test to render into the SVG Graphics2D implementation.
    // val test = new TestSVGGen()
    paint(svgGenerator)
    // Finally, stream out SVG to the standard output using
    // UTF-8 encoding.
    val useCSS = true
    // we want to use CSS style attributes
    val file = new File("dramp.svg")
    val out = new OutputStreamWriter(new FileOutputStream(file), "UTF-8")
    svgGenerator.stream(out, useCSS)
  }


}