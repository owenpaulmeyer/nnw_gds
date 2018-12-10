package net.crinklejoint

import net.crinklejoint.SVGParser.{parsePathShape, _}
import org.scalatest.{FunSpec, Matchers}

class SVGParserSpec extends FunSpec with Matchers {

  describe("SVG io") {
    describe("readFile and parseNode") {
      it("should input a file path and produce a dom which parseNode should convert into a path string") {
        val path = "src/test/resources/inkscape_sample.svg"
        val dom = readFile(path)
        val expectedPathString = List(
          "m 26.458333,196.9875 -3.96875,-3.57188 3.96875,-3.57187 h -7.9375 l 2.645834,3.57188 -2.645834,3.57187",
          "m 10.583333,204.13125 h 14.552083 v 9.525 l -6.614583,-4.7625 h -7.9375 z")
        parseNode(dom) shouldBe expectedPathString
      }
    }
    describe("parsePathShape createSVGPath") {
      it("should produce a pathIterator from an svg path string") {
        val pathStrings = List(
          "m 26.458333,196.9875 -3.96875,-3.57188 3.96875,-3.57187 h -7.9375 l 2.645834,3.57188 -2.645834,3.57187",
          "m 10.583333,204.13125 h 14.552083 v 9.525 l -6.614583,-4.7625 h -7.9375 z"
        )

        val pathIterators = parsePathShape(pathStrings)

        val expectedPathString = "M26.4583 196.9875 L22.4896 193.4156 L26.4583 189.8438 L18.5208 189.8438 L21.1667 193.4156 L18.5208 196.9875 M10.5833 204.1313 L25.1354 204.1313 L25.1354 213.6562 L18.5208 208.8938 L10.5833 208.8938 Z"
        createSVGPath(pathIterators) shouldBe expectedPathString
      }
      ignore("should close shapes") {
        val pathData = "m 10.583333,204.13125 h 14.552083 v 9.525 l -6.614583,-4.7625 h -7.9375 z"
        val pathIterators = parsePathShape(List(pathData))
        val pathIteratorsCopy = parsePathShape(List(pathData))
        val coordinates = generateCoordinates(pathIterators)
        val pathIteratorsOutp = parsePoints(coordinates)
        val alsoCoordinates = generateCoordinates(pathIteratorsOutp)
        val comparePath = createSVGPath(pathIteratorsOutp)


        val path = createSVGPath(pathIteratorsCopy)
        path
        "M10.5833 204.1313 L25.1354 204.1313 L25.1354 213.6562 L18.5208 208.8938 L10.5833 208.8938 L10.5833 208.8938"
        "M10.5833 204.1313 L25.1354 204.1313 L25.1354 213.6562 L18.5208 208.8938 L10.5833 208.8938 Z"

        val a = parsePathShape(List(pathData))
        val b = parsePoints(coordinates)
        val ls = List(a,b)
        ls
      }
    }

    describe("parsePoints") {
      it("should take a sequence of (float, float) and return a pathIterator") {
        val points = List(
          Segment(Point(26.458332f, 196.9875f),  0),
          Segment(Point(26.458332f, 196.9875f),  1),
          Segment(Point(22.489582f, 193.41562f), 1),
          Segment(Point(26.458332f, 189.84375f), 1),
          Segment(Point(18.520832f, 189.84375f), 1),
          Segment(Point(22.489582f, 193.41562f), 1),
          Segment(Point(18.520832f, 196.9875f),  1)
        )

        val pathIterators = SVGParser.parsePoints(List(points))
        val expectedPath = "M26.4583 196.9875 L26.4583 196.9875 L22.4896 193.4156 L26.4583 189.8438 L18.5208 189.8438 L22.4896 193.4156 L18.5208 196.9875"
        createSVGPath(pathIterators) shouldBe expectedPath
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
        val pathIterators = parsePathShape(List(
          "m 27.78125,107.69063 -3.96875,4.7625 -3.96875,-4.7625 0,8.33437 3.96875,-4.7625 3.96875,4.7625",
          "m 10.583333,204.13125 h 14.552083 v 9.525 l -6.614583,-4.7625 h -7.9375 z"
        ))

        val expectedPath = "M27.7812 107.6906 L23.8125 112.4531 L19.8438 107.6906 L19.8438 116.025 L23.8125 111.2625 L27.7812 116.025 M10.5833 204.1313 L25.1354 204.1313 L25.1354 213.6562 L18.5208 208.8938 L10.5833 208.8938 Z"
        createSVGPath(pathIterators) shouldBe expectedPath
      }
    }

    it("should produce a list of coordinates from a file") {
      val node = readFile("src/test/resources/output.svg")
      val pathStrings = parseNode(node)
      val pathIterators = parsePathShape(pathStrings)

      val expectedCoordinates = List(
        Segment(Point(26.4583f,196.9875f), 0),
        Segment(Point(26.4583f,196.9875f), 1),
        Segment(Point(22.4896f,193.4156f), 1),
        Segment(Point(26.4583f,189.8438f), 1),
        Segment(Point(18.5208f,189.8438f), 1),
        Segment(Point(22.4896f,193.4156f), 1),
        Segment(Point(18.5208f,196.9875f), 1)
      )
      generateCoordinates(pathIterators) shouldBe List(expectedCoordinates)
    }


    describe("back and forth") {
      it("should read a file into a list of lists of floats and back into a file") {
        val path = "src/test/resources/sigmoid.svg"
//        val path = "src/test/resources/inkscape_sample.svg"
//        val path = "src/test/resources/hex_grid.svg"
        val domInp = readFile(path)
        val pathInps = parseNode(domInp)
        val pathIteratorInps = parsePathShape(pathInps)
        val coordinates = generateCoordinates(pathIteratorInps)
        println(s">>>")
        coordinates map (cs => println(s"$cs"))

        val pathIteratorOutps = SVGParser.parsePoints(coordinates)
        val pathOutps = createSVGPath(pathIteratorOutps)
        val domOutps = buildDocument(pathOutps)
        writeToFile(domOutps, "src/test/resources/outputFull.svg")
      }


      it("should convert several list of segments to an svg") {
        val coordinates = List(
          List(
            Segment(Point(105.83333f, 77.925f), 0),
            Segment(Point(79.375f, 111.2625f), 1),
            Segment(Point(52.916664f, 77.925f), 1)),
          List(
            Segment(Point(105.83333f, 139.8375f), 0),
            Segment(Point(52.916664f, 139.8375f), 1),
            Segment(Point(79.375f, 111.2625f), 1),
            Segment(Point(105.83333f, 139.8375f), 1))
        )

        val pathIteratorOutps = SVGParser.parsePoints(coordinates)
        val pathOutps = createSVGPath(pathIteratorOutps)
        val domOutps = buildDocument(pathOutps)
        writeToFile(domOutps, "src/test/resources/outputX.svg"
        )
      }
    }
  }
}
