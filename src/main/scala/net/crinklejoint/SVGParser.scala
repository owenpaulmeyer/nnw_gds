package net.crinklejoint

import java.awt.{Color, Graphics2D}
import java.awt.geom.{GeneralPath, PathIterator}
import java.io.{File, FileOutputStream, IOException, OutputStreamWriter}
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import javax.xml.xpath.{XPathConstants, XPathFactory}
import org.apache.batik.anim.dom.SVGDOMImplementation
import org.apache.batik.dom.GenericDOMImplementation
import org.apache.batik.parser.{FloatArrayProducer, PointsParser}
import org.apache.batik.svggen.SVGGraphics2D
import org.w3c.dom
import org.w3c.dom.{Node, NodeList}
import org.apache.batik.parser.{AWTPathProducer, PathParser}
import scala.collection.Seq


object SVGParser {
  def readFile(path: String): dom.Document = {
    val factory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance
    val builder: DocumentBuilder = factory.newDocumentBuilder
    val document: dom.Document = builder.parse(path)
    document
  }

  def parseNode(document: dom.Document) = {
    val xpathExpression = "//path/@d"
    val xpf = XPathFactory.newInstance
    val xpath = xpf.newXPath
    val expression = xpath.compile(xpathExpression)
    val svgPaths = expression.evaluate(document, XPathConstants.NODESET).asInstanceOf[NodeList]
    val node = svgPaths.item(0).getNodeValue
    node
  }

  def parsePathShape(svgPathShape: String): PathIterator = try {
    val pathProducer = new AWTPathProducer
    val pathParser = new PathParser
    pathParser.setPathHandler(pathProducer)
    pathParser.parse(svgPathShape)
    val shape = pathProducer.getShape
    val affineTransform = null
    val pathIterator = shape.getPathIterator(affineTransform)
    pathIterator
  }

  def parsePoints(points: Seq[(Float, Float)]) = {
    val start = points.head
    val path  = points.tail

    val pathProducer = new AWTPathProducer
    pathProducer.startPath()
    (pathProducer.movetoAbs _).tupled(start)
    for (point <- path) (pathProducer.linetoAbs _).tupled(point)
    pathProducer.endPath
    val shape = pathProducer.getShape
    val affineTransform = null
    shape.getPathIterator(affineTransform)
  }

  def createSVGPath(paths: Seq[PathIterator]) = {
    val domImpl = GenericDOMImplementation.getDOMImplementation
    val svgNS = null
    val document = domImpl.createDocument(svgNS, "assvg", null)

    val generalPath = new GeneralPath
    for (path <- paths) generalPath.append(path, false)

    val svgGenerator = new SVGGraphics2D(document)
    svgGenerator.getShapeConverter.toSVG(generalPath).getAttribute("d")
  }

  def buildDocument(dAttribute: String)=  {
    val impl = SVGDOMImplementation.getDOMImplementation
    val svgNS = SVGDOMImplementation.SVG_NAMESPACE_URI
    val doc = impl.createDocument(svgNS, "svg", null)
    // get the root element (the svg element)
    val svgRoot = doc.getDocumentElement
    svgRoot.setAttribute("width", "210mm")
    svgRoot.setAttribute("height", "297mm")
    svgRoot.setAttribute("viewBox", "0 0 210 297")
    svgRoot.setAttribute("version", "1.1")
    svgRoot.setAttribute("id", "svg8")
    svgRoot.setAttribute("viewBox", "0 0 210 297")
    svgRoot.setAttribute("viewBox", "0 0 210 297")

    val element = doc.createElementNS(svgNS, "path")
    element.setAttribute("d", dAttribute)
    element.setAttribute("style",
      """fill:none;
        |stroke:#000000;
        |stroke-width:0.26458332px;
        |stroke-linecap:butt;
        |stroke-linejoin:miter;
        |stroke-opacity:1""".stripMargin)
    svgRoot.appendChild(element)
    doc
  }

  def writeToFile(node: Node, path: String): Unit = {
    import java.io.FileWriter
    import javax.xml.transform.TransformerFactory
    import javax.xml.transform.dom.DOMSource
    import javax.xml.transform.stream.StreamResult

    val source = new DOMSource(node)
    val writer = new FileWriter(new File(path))
    val result = new StreamResult(writer)

    val transformerFactory = TransformerFactory.newInstance
    val transformer = transformerFactory.newTransformer

    import javax.xml.transform.OutputKeys
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no")
    transformer.setOutputProperty(OutputKeys.METHOD, "xml")
    transformer.setOutputProperty(OutputKeys.INDENT, "yes")
    transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
    transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "UTF-8")
    transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4")

    transformer.transform(source, result)
  }













//  def pointsToPath(points: Seq[Float]) = {
//    val pathProducer = new AWTPathProducer
//    val pathParser = new PathParser
//    pathParser.setPathHandler(pathProducer)
////    pathParser.parse(svgPathShape)
//    val shape = pathProducer.getShape
//    val pathIterator = shape.getPathIterator(null)
//    pathIterator
//  }

















  def paint(g2d: Graphics2D, paths: Seq[GeneralPath]): Unit = {
    g2d.setPaint(Color.red)
    val path = new GeneralPath
    val iterator = parsePathShape("m 27.78125,107.69063 -3.96875,4.7625 -3.96875,-4.7625 0,8.33437 3.96875,-4.7625 3.96875,4.7625")
    path.append(iterator, false)
//
//    val p2 = new GeneralPath
//    val i2 = parsePathShape("m 10.583333,204.13125 h 14.552083 v 9.525 l -6.614583,-4.7625 h -7.9375 z")
//    p2.append(i2, false)

    for (path <- paths) g2d.draw(path)
  }

  @throws[IOException]
  def createSvgGenerator(document: dom.Document, paths: Seq[GeneralPath]): Unit = {
    // Get a DOMImplementation.
//    val domImpl = GenericDOMImplementation.getDOMImplementation
    // Create an instance of org.w3c.dom.Document.
//    val svgNS = "file:///home/owenpaulmeyer/src/nnw_gds/ver_simple.svg"
//    val document = domImpl.createDocument(svgNS, "svg", null)
    // Create an instance of the SVG Generator.
    val svgGenerator: SVGGraphics2D = new SVGGraphics2D(document)
    // Ask the test to render into the SVG Graphics2D implementation.
    paint(svgGenerator, paths)
    svgGenerator

    // Finally, stream out SVG to the standard output using
    // UTF-8 encoding.
//    val useCSS = false
    // we want to use CSS style attributes
//    val file = new File("dramp.svg")
//    val out = new OutputStreamWriter(new FileOutputStream(file), "UTF-8")
//    svgGenerator.stream(out, useCSS)
  }
  def writeOut(svgGenerator: SVGGraphics2D, file: File): Unit = {
    val useCSS = false
    val out = new OutputStreamWriter(new FileOutputStream(file), "UTF-8")
    svgGenerator.stream(out, useCSS)
  }


}