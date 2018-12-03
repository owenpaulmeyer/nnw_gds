package svg

import net.crinklejoint.SVGParser
import net.crinklejoint.SVGParser.{parsePathShape, _}
import org.scalatest.{FunSpec, Matchers}
import scala.collection._

class SVGParserSpec extends FunSpec with Matchers {

  describe("SVG io") {
    describe("readFile and parseNode") {
      it("should input a file path and produce a dom which parseNode should convert into a path string") {
        val path = "src/test/resources/inkscape_sample.svg"
        val dom = readFile(path)
        val expectedPathString = "m 26.458333,196.9875 -3.96875,-3.57188 3.96875,-3.57187 h -7.9375 l 2.645834,3.57188 -2.645834,3.57187"
        parseNode(dom) shouldBe expectedPathString
      }
    }
    describe("parsePathShape createSVGPath") {
      it("should produce a pathIterator from an svg path string") {
        val pathString1 = "m 26.458333,196.9875 -3.96875,-3.57188 3.96875,-3.57187 h -7.9375 l 2.645834,3.57188 -2.645834,3.57187"
        val pathString2 = "m 10.583333,204.13125 h 14.552083 v 9.525 l -6.614583,-4.7625 h -7.9375 z"

        val pI1 = parsePathShape(pathString1)
        val pI2 = parsePathShape(pathString2)

        val expectedPathString = "M26.4583 196.9875 L22.4896 193.4156 L26.4583 189.8438 L18.5208 189.8438 L21.1667 193.4156 L18.5208 196.9875 M10.5833 204.1313 L25.1354 204.1313 L25.1354 213.6562 L18.5208 208.8938 L10.5833 208.8938 Z"
        createSVGPath(Seq(pI1, pI2)) shouldBe expectedPathString
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
        createSVGPath(Seq(pathIterator)) shouldBe expectedPath
      }
    }

    describe("buildDocument writeToFile") {
      it("should construct a dom for export to file") {
        val pathData = "M26.4583 196.9875 L26.4583 196.9875 L22.4896 193.4156 L26.4583 189.8438 L18.5208 189.8438 L22.4896 193.4156 L18.5208 196.9875"
        val dom = buildDocument(pathData)
        writeToFile(dom, "src/test/resources/output.svg")
      }
    }

    describe("createPath") {
      it("should take a Seq of Paths and return an svg path string") {
        val pI1 = parsePathShape(
        "m 27.78125,107.69063 -3.96875,4.7625 -3.96875,-4.7625 0,8.33437 3.96875,-4.7625 3.96875,4.7625")
        val pI2 = parsePathShape(
        "m 10.583333,204.13125 h 14.552083 v 9.525 l -6.614583,-4.7625 h -7.9375 z")

        val expectedPath = "M27.7812 107.6906 L23.8125 112.4531 L19.8438 107.6906 L19.8438 116.025 L23.8125 111.2625 L27.7812 116.025 M10.5833 204.1313 L25.1354 204.1313 L25.1354 213.6562 L18.5208 208.8938 L10.5833 208.8938 Z"
        createSVGPath(Seq(pI1, pI2)) shouldBe expectedPath
      }
    }

    it("should would") {
      val node = readFile("src/test/resources/output.svg")
      val pathString = parseNode(node)
      val pathIterator = parsePathShape(pathString)

      val expectedCoordinates = List(
        (26.4583f,196.9875f),
        (26.4583f,196.9875f),
        (22.4896f,193.4156f),
        (26.4583f,189.8438f),
        (18.5208f,189.8438f),
        (22.4896f,193.4156f),
        (18.5208f,196.9875f)
      )
      generateCoordinates(pathIterator) shouldBe expectedCoordinates
    }

  }
}
