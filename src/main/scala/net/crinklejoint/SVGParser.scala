package net.crinklejoint

import java.awt.geom.{GeneralPath, PathIterator}
import java.io.File
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import javax.xml.xpath.{XPathConstants, XPathFactory}
import org.apache.batik.anim.dom.SVGDOMImplementation
import org.apache.batik.dom.GenericDOMImplementation
import org.apache.batik.parser.{AWTPathProducer, PathParser}
import org.apache.batik.svggen.SVGGraphics2D
import org.w3c.dom
import org.w3c.dom.{Node, NodeList}


object SVGParser extends Graph with Protocol {
  def readFile(filePath: String): dom.Document = {
    val factory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance
    val builder: DocumentBuilder = factory.newDocumentBuilder
    val document: dom.Document = builder.parse(filePath)
    document
  }

  def parseNode(document: dom.Document): List[String] = {
    val xpathExpression = "//path/@d"
    val xpf = XPathFactory.newInstance
    val xpath = xpf.newXPath
    val expression = xpath.compile(xpathExpression)
    val svgPaths = expression.evaluate(document, XPathConstants.NODESET).asInstanceOf[NodeList]
    val nodeCount = svgPaths.getLength
    val ls = List.range(0, nodeCount)
    ls map { idx => svgPaths.item(idx).getNodeValue }
  }

  def parsePathShape(svgPathShapes: List[String]): List[PathIterator] = try {
    svgPathShapes map { svgPathShape =>
      val pathProducer = new AWTPathProducer
      val pathParser = new PathParser
      pathParser.setPathHandler(pathProducer)
      pathParser.parse(svgPathShape)
      val shape = pathProducer.getShape
      val affineTransform = null
      val pathIterator = shape.getPathIterator(affineTransform)
      pathIterator
    }
  }

  def generateCoordinates(pathIterators: List[PathIterator]): List[List[Segment]] = {
    pathIterators map { pathIterator =>
      val buffer = scala.collection.mutable.ArrayBuffer.empty[Segment]
      var coords: Array[Float] = new Array[Float](6)
      while (!pathIterator.isDone) {
        val t = pathIterator.currentSegment(coords)
        buffer.append(Segment(Point(coords(0), coords(1)), t))
        pathIterator.next()
        "next"
      }
      buffer.toList
    }
  }

  def parsePoints(coordinates: List[List[Segment]]): List[PathIterator] = {
    coordinates map { segments =>
      val start = segments.head
      val path = segments.tail

      val pathProducer = new AWTPathProducer
      pathProducer.startPath()

      def produceSegment(seg: Segment) {
        seg match {
          case Segment(Point(a, b), SEG_CLOSE)  => pathProducer.closePath()
          case Segment(Point(a, b), SEG_MOVETO) => pathProducer.movetoAbs(a, b)
          case Segment(Point(a, b), SEG_LINETO) => pathProducer.linetoAbs(a, b)
        }
      }

      produceSegment(start)
      for (point <- path) produceSegment(point)

      pathProducer.endPath

      val shape = pathProducer.getShape
      val affineTransform = null
      shape.getPathIterator(affineTransform)
    }
  }

  def createSVGPath(paths: List[PathIterator]) = {
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
}