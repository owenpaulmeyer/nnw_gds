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

    it("graph ") {
      val path1                : List[Segment] = List(
        Segment(Point(10.583333f,204.13126f),0),
        Segment(Point(25.135416f,204.13126f),1),
        Segment(Point(25.135416f,213.65625f),1),
        Segment(Point(18.520832f,208.89375f),1),
        Segment(Point(10.583332f,208.89375f),1),
        Segment(Point(10.583332f,208.89375f),4)
      )
      val path2                : List[Segment] = List(
        Segment(Point(26.458332f,196.9875f),0),
        Segment(Point(22.489582f,193.41562f),1),
        Segment(Point(26.458332f,189.84375f),1),
        Segment(Point(18.520832f,189.84375f),1),
        Segment(Point(21.166666f,193.41563f),1),
        Segment(Point(18.520832f,196.9875f),1)
      )
      val coordinates          : List[List[Segment]] = List(path1,path2)
      val graph                = coordinatesToGraph(coordinates: List[List[Segment]])
      graph2coordinates(graph)
//      println(s">>>>>")
//      println(s"$graph")

      println(s">>>")
//      export(graph)
    }
    it("more") {
      val path1                = List(
        Segment(Point(52.916664f,77.925f),0),
        Segment(Point(79.375f,111.2625f),1),
        Segment(Point(105.83333f,77.925f),1)
      )
      val path2                = List(
        Segment(Point(79.375f,111.2625f),0),
        Segment(Point(105.83333f,139.8375f),1),
        Segment(Point(52.916664f,139.8375f),1),
        Segment(Point(52.916664f,139.8375f),4)
      )
      List(
        List(
          Segment(Point(105.83333f,77.925f),0),
          Segment(Point(79.375f,111.2625f),1),
          Segment(Point(52.916664f,77.925f),1)),
        List(
          Segment(Point(105.83333f,139.8375f),0),
          Segment(Point(52.916664f,139.8375f),1),
          Segment(Point(79.375f,111.2625f),1),
          Segment(Point(105.83333f,139.8375f),1)))

      val result               = List(
        (Point(105.83333f,   77.925f),  null),
        (Point( 79.375f,    111.2625f), Point(105.83333f, 77.925f)),
        (Point( 52.916664f,  77.925f),
                                        Point(79.375f,   111.2625f)),
        (Point(105.83333f,  139.8375f), Point(79.375f,   111.2625f)),
        (Point( 52.916664f, 139.8375f), Point(79.375f,   111.2625f))
      )

      val jointed              = joint(result)

//      println(s"jointed: $jointed")

      val coordinates          : List[List[Segment]] = List(path1, path2)
      val graph                = coordinatesToGraph(coordinates: List[List[Segment]])
      graph2coordinates(graph)
    }
    it("more2") {
      val path1                = List(
        Segment(Point(52.916664f,77.925f),0),
        Segment(Point(79.375f,111.2625f),1)
      )
      val ls                   = List(
        (Point(52.916664f,77.925f),null),
        (Point(79.375f,111.2625f),Point(52.916664f,77.925f))
      )


      val coordinates          : List[List[Segment]] = List(path1)
      val graph                = coordinatesToGraph(coordinates: List[List[Segment]])
      graph2coordinates(graph)
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
//      println(s">>>$coordinatesReturn")
//      val jointed = joint(coordinatesReturn, null)

//      coordinates map (cs => println(s"$cs"))
//      println(s">>>")
//      println(coordinatesReturn)

      val pathIteratorOutps    = SVGParser.parsePoints(coordinatesReturn)
      val pathOutps            = createSVGPath(pathIteratorOutps)
      val domOutps             = buildDocument(pathOutps)
      writeToFile(domOutps, "src/test/resources/outputXFull.svg")
    }
  }
  val coordinatesOG = List(
    List(
      Segment(Point(67.46875f,108.88125f),0),
      Segment(Point(70.114586f,108.88125f),1),
      Segment(Point(67.46875f,111.2625f),1),
      Segment(Point(67.46875f,108.88125f),1)),
    List(
      Segment(Point(67.46875f,111.2625f),0),
      Segment(Point(64.822914f,111.2625f),1),
      Segment(Point(64.822914f,113.643745f),1),
      Segment(Point(67.46875f,113.643745f),1),
      Segment(Point(67.46875f,113.643745f),4)),
    List(
      Segment(Point(67.46875f,111.2625f),0),
      Segment(Point(70.114586f,113.643745f),1),
      Segment(Point(72.76042f,111.2625f),1),
      Segment(Point(72.76042f,111.2625f),4)),
    List(
      Segment(Point(67.46875f,111.2625f),0),
      Segment(Point(64.822914f,108.88125f),1)),
    List(
      Segment(Point(67.46875f,108.88125f),0),
      Segment(Point(67.46875f,106.5f),1),
      Segment(Point(70.114586f,106.5f),1)),
    List(
      Segment(Point(70.114586f,113.64375f),0),
      Segment(Point(70.114586f,116.025f),1),
      Segment(Point(67.46875f,116.025f),1)))

  List(
    Map(
      Point(64.822914f,108.88125f)  -> (null,                        0),
      Point(67.46875f,111.2625f)    -> (Point(64.822914f,108.88125f),1),
      Point(72.76042f,111.2625f)    -> (Point(67.46875f,111.2625f),  2),
      Point(67.46875f,108.88125f)   -> (Point(67.46875f,111.2625f),  2),
      Point(70.114586f,113.643745f) -> (Point(67.46875f,111.2625f),  2),
      Point(64.822914f,111.2625f)   -> (Point(67.46875f,111.2625f),  2),
      Point(67.46875f,113.643745f)  -> (Point(67.46875f,111.2625f),  2),
      Point(70.114586f,108.88125f)  -> (Point(67.46875f,111.2625f),  2),
      Point(67.46875f,106.5f)       -> (Point(67.46875f,108.88125f), 3),
      Point(64.822914f,113.643745f) -> (Point(64.822914f,111.2625f), 3),
      Point(70.114586f,106.5f)      -> (Point(67.46875f,106.5f),     4)
    ),
    Map(
      Point(70.114586f, 113.64375f) -> (null,                          0),
      Point(70.114586f, 116.025f)   -> (Point(70.114586f, 113.64375f), 1),
      Point(67.46875f, 116.025f)    -> (Point(70.114586f, 116.025f),   2)
    )
  )

  List(
  List(
    (Point(64.822914f,108.88125f),  null),
    (Point(67.46875f,111.2625f),    Point(64.822914f,108.88125f)),
    (Point(70.114586f,108.88125f),  Point(67.46875f,111.2625f)),
    (Point(67.46875f,108.88125f),   Point(67.46875f,111.2625f)),
    (Point(64.822914f,111.2625f),   Point(67.46875f,111.2625f)),
    (Point(67.46875f,113.643745f),  Point(67.46875f,111.2625f)),
    (Point(70.114586f,113.643745f), Point(67.46875f,111.2625f)),
    (Point(72.76042f,111.2625f),    Point(67.46875f,111.2625f)),
    (Point(67.46875f,106.5f),       Point(67.46875f,108.88125f)),
    (Point(64.822914f,113.643745f), Point(64.822914f,111.2625f)),
    (Point(70.114586f,106.5f),      Point(67.46875f,106.5f))),
  List(
    (Point(70.114586f,113.64375f),  null),
    (Point(70.114586f,116.025f),    Point(70.114586f,113.64375f)),
    (Point(67.46875f,116.025f),     Point(70.114586f,116.025f)))
  )

}