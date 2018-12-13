package net.crinklejoint

import java.awt.geom.Point2D
import net.crinklejoint.SVGParser.{buildDocument, createSVGPath, generateCoordinates, parseNode, parsePathShape, readFile, writeToFile}
import org.scalatest.{FunSpec, Matchers}
import org.jgrapht.graph.{DefaultWeightedEdge, SimpleWeightedGraph}

import Protocol._

class GraphDSpec extends FunSpec with Matchers with GraphD {

  describe("Graph") {
    it("should interchange case class Point and java.awt.geom.Point2D.Float") {
      val pointA               = Point(1f,2f)
      val pointB               = Point(1f,4f)
      println(s">>> ${pointA.distance(pointB)}")
      def f(p                  : Point) = {
        println(s"point $p")
      }
      println("<<<<<<<<<<<<")
      val p1                   = new Point2D.Float(1f,2f)
      val p2                   = new Point2D.Float(1f,2f)
//      f(p1)
      implicitly[Point](p1) match {
        case Point(a,b) => println(s"a: $a, b: $b")
      }
      println(s">>>>>> ${pointA == implicitly[Point](pointB)}")
      println(s":::: ${p1.distance(pointB)}")

      val x                    : Any = 3
      x match {
        case a:String => println(""" "3"! """)
        case a:Int   => println(s" Three! $a ")
      }
    }
    it("graph test") {
      val g: SimpleWeightedGraph[Point, DefaultWeightedEdge] =
        new SimpleWeightedGraph[Point, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
      val p1                   = Point(1,2)
      val p2                   = Point(1,2)
      println(s">>>> $g")
      g.addVertex(p1)
      g.addVertex(p2)
      g.addVertex(p1)
      g.addVertex(p2)
      g.addVertex(p2)
      println(s">>>> $g")
    }

    it("should read a file into graph and back into a file") {
      val path                 = "src/test/resources/sigmoid.svg"
      //        val path = "src/test/resources/inkscape_sample.svg"
//        val path = "src/test/resources/hex_grid.svg"
      val domInp               = readFile(path)
      val pathInps             = parseNode(domInp)
      val pathIteratorInps     = parsePathShape(pathInps)
      val coordinates          = generateCoordinates(pathIteratorInps)
      val graph                = coordinatesToGraph(coordinates)

      val coordinatesReturn    = graphToCoordinates(graph)
      val pathIteratorOutps    = SVGParser.parsePoints(coordinatesReturn)
      val pathOutps            = createSVGPath(pathIteratorOutps)
      val domOutps             = buildDocument(pathOutps)
      writeToFile(domOutps, "src/test/resources/outputXFull.svg")
    }
  }
}