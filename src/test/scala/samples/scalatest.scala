package samples


import java.awt.geom.{GeneralPath, PathIterator}
import java.io.File
import net.crinklejoint.SVGParser
import net.crinklejoint.SVGParser.parsePathShape
import org.apache.batik.anim.dom.SVGDOMImplementation
import org.apache.batik.dom.GenericDOMImplementation
import org.apache.batik.parser.{AWTPathProducer, FloatArrayProducer, PointsParser}
import org.apache.batik.svggen.SVGGraphics2D
import org.scalatest.{FunSpec, Matchers}
import org.w3c.dom.Node
import scala.collection._

class SVGParserSpec extends FunSpec with Matchers {

  describe("svg file io") {

    it("should read an svg into a sequence of coordinates") {
      val document = SVGParser.readFile("ver_simple.svg")
      val node = SVGParser.parseNode(document)
      val iterator = SVGParser.parsePathShape(node)

      var coords: Array[Float] = new Array[Float](6)
      while (!iterator.isDone) {

//        """
//          |>> (26.458332,196.9875)
//          |>> (22.489582,193.41562)
//          |>> (26.458332,189.84375)
//          |>> (18.520832,189.84375)
//          |>> (22.489582,193.41562)
//          |>> (18.520832,196.9875)
//        """.stripMargin



        iterator.currentSegment(coords)
        println(s">> ${(coords(0), coords(1))}")
        iterator.next()

        "next"
      }
    }

    describe("parsePoints") {
      it("should take a sequence of (float, float) and return a pathIterator") {
        val points = Seq(
          (26.458332f, 196.9875f),
          (26.458332f, 196.9875f),
          (22.489582f, 193.41562f),
          (26.458332f, 189.84375f),
          (18.520832f, 189.84375f),
          (22.489582f, 193.41562f),
          (18.520832f, 196.9875f)
        )

        val pathIterator = SVGParser.parsePoints(points)
        val expectedPath = "M26.4583 196.9875 L26.4583 196.9875 L22.4896 193.4156 L26.4583 189.8438 L18.5208 189.8438 L22.4896 193.4156 L18.5208 196.9875"
        SVGParser.createSVGPath(Seq(pathIterator)) shouldBe expectedPath
      }
    }

    it("should export to disk") {

      // Get a DOMImplementation.
//    val domImpl = GenericDOMImplementation.getDOMImplementation
      // Create an instance of org.w3c.dom.Document.
//    val svgNS = "file:///home/owenpaulmeyer/src/nnw_gds/ver_simple.svg"
//    val document = domImpl.createDocument(svgNS, "svg", null)
      // Create an instance of the SVG Generator.


      val domImpl = GenericDOMImplementation.getDOMImplementation
      // Create an instance of org.w3c.dom.Document.
      val svgNS = "file:///home/owenpaulmeyer/src/nnw_gds/dramp2.svg"
      val document = domImpl.createDocument(svgNS, "svg", null)

      val documentRead = SVGParser.readFile("ver_simple.svg")
      // Create an instance of the SVG Generator.
      val svgGenerator = new SVGGraphics2D(document)

      val file = new File("dramp2.svg")

      //      g2d.setPaint(Color.red)
      val path = new GeneralPath
      val iterator = SVGParser.parsePathShape("m 27.78125,107.69063 -3.96875,4.7625 -3.96875,-4.7625 0,8.33437 3.96875,-4.7625 3.96875,4.7625")
      path.append(iterator, false)

      SVGParser.createSvgGenerator(document, Seq(path))
      SVGParser.writeOut(svgGenerator, file)
    }
    describe("createPath") {
      it("should take a Seq of Paths and return an svg path string") {
        val pI1 = parsePathShape(
        "m 27.78125,107.69063 -3.96875,4.7625 -3.96875,-4.7625 0,8.33437 3.96875,-4.7625 3.96875,4.7625")
        val pI2 = parsePathShape(
        "m 10.583333,204.13125 h 14.552083 v 9.525 l -6.614583,-4.7625 h -7.9375 z")

        val expectedPath = "M27.7812 107.6906 L23.8125 112.4531 L19.8438 107.6906 L19.8438 116.025 L23.8125 111.2625 L27.7812 116.025 M10.5833 204.1313 L25.1354 204.1313 L25.1354 213.6562 L18.5208 208.8938 L10.5833 208.8938 Z"
        SVGParser.createSVGPath(Seq(pI1, pI2)) shouldBe expectedPath
      }
    }
    it("split") {
      val domImpl = GenericDOMImplementation.getDOMImplementation
      val svgNS = null
      val document = domImpl.createDocument(svgNS, "svg", null)
      val pathShapeIterator = parsePathShape(
        "m 27.78125,107.69063 -3.96875,4.7625 -3.96875,-4.7625 0,8.33437 3.96875,-4.7625 3.96875,4.7625")
      val pathShapeIterator2 = parsePathShape(
        "m 10.583333,204.13125 h 14.552083 v 9.525 l -6.614583,-4.7625 h -7.9375 z")

//      val document = SVGParser.readFile("ver_simple.svg")
      def createPath(paths: Seq[PathIterator]) = {
        val generalPath = new GeneralPath
        for (path <- paths) generalPath.append(path, false)

        val svgGenerator = new SVGGraphics2D(document)
        svgGenerator.getShapeConverter.toSVG(generalPath).getAttribute("d")
      }


//      def buildDocument(paths: Seq[String])=  {
//        val impl = SVGDOMImplementation.getDOMImplementation
//        val svgNS = SVGDOMImplementation.SVG_NAMESPACE_URI
//        val doc = impl.createDocument(svgNS, "svg", null)
//        // get the root element (the svg element)
//        val svgRoot = doc.getDocumentElement
//        svgRoot.setAttribute("width", "210mm")
//        svgRoot.setAttribute("height", "297mm")
//        svgRoot.setAttribute("viewBox", "0 0 210 297")
//        svgRoot.setAttribute("version", "1.1")
//        svgRoot.setAttribute("id", "svg8")
//        svgRoot.setAttribute("viewBox", "0 0 210 297")
//        svgRoot.setAttribute("viewBox", "0 0 210 297")
//        // create the path element
//        val element = doc.createElementNS(svgNS, "path")
//        element.setAttribute("d",
//          "m 26.458333,196.9875 -3.96875,-3.57188 3.96875,-3.57187 h -7.9375 l 2.645834,3.57188 -2.645834,3.57187")
//        element.setAttribute("style",
//          "fill:none;stroke:#000000;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1")
//        svgRoot.appendChild(element)
//        doc
//      }
      val doc = SVGParser.buildDocument("m 26.458333,196.9875 -3.96875,-3.57188 3.96875,-3.57187 h -7.9375 l 2.645834,3.57188 -2.645834,3.57187")




//      val svgGenerator = createSvgGenerator(document, Seq(pathShapeIterator))
      val node = createPath(Seq(pathShapeIterator, pathShapeIterator2))


      val svgRoot = document.getDocumentElement

// set the width and height attribute on the root svg element
      svgRoot.setAttributeNS(null, "width", "210mm")
      svgRoot.setAttributeNS(null, "height", "297mm")
      svgRoot.setAttributeNS(null, "viewBox", "0 0 210 297")
      // create the rectangle
      val rectangle = document.createElementNS(null, "g")
      rectangle.setAttributeNS(null, "x", "10")
      rectangle.setAttributeNS(null, "y", "20")

//      rectangle.setAttributeNS(null, "path",
//        "m 10.583333,204.13125 h 14.552083 v 9.525 l -6.614583,-4.7625 h -7.9375 z")
//      rectangle.setAttributeNS("g", "path", "wrtath")

      // attach the rectangle to the svg root element
      val generalPath = new GeneralPath
      generalPath.append(pathShapeIterator, false)

      val svgGenerator = new SVGGraphics2D(document)
      svgGenerator.draw(generalPath)
      svgRoot.appendChild(rectangle)







//      def writeToFile(node: Node, path: String): Unit = {
//        import java.io.FileWriter
//        import javax.xml.transform.TransformerFactory
//        import javax.xml.transform.dom.DOMSource
//        import javax.xml.transform.stream.StreamResult
//
//        val source = new DOMSource(node)
//        val writer = new FileWriter(new File(path))
//        val result = new StreamResult(writer)
//
//        val transformerFactory = TransformerFactory.newInstance
//        val transformer = transformerFactory.newTransformer
//
//        import javax.xml.transform.OutputKeys
//        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no")
//        transformer.setOutputProperty(OutputKeys.METHOD, "xml")
//        transformer.setOutputProperty(OutputKeys.INDENT, "yes")
//        transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
//        transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "UTF-8")
//        transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4")
//
//        transformer.transform(source, result)
//      }
      SVGParser.writeToFile(document, "trance2.svg")
      SVGParser.writeToFile(doc, "doc.svg")


//      SVGParser.paint(svgGenerator)
      // Finally, stream out SVG to the standard output using
      // UTF-8 encoding.
//      val useCSS = false
      // we want to use CSS style attributes
//      val file = new File("dramp.svg")
//      val out = new OutputStreamWriter(new FileOutputStream(file), "UTF-8")
//      svgGenerator.stream(out, useCSS)


//      svgGenerator.stream("dramp_stamp.svg")
    }
  }
}
