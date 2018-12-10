package net.crinklejoint

import java.awt.geom.Point2D
import org.scalatest.{FunSpec, Matchers}
import org.jgrapht.graph.{DefaultWeightedEdge, SimpleWeightedGraph}


class GraphDSpec extends FunSpec with Matchers with Protocol with GraphD {

  describe("Graph") {
    it("should interchange case class Point and java.awt.geom.Point2D.Float") {
      val pointA = Point(1f,2f)
      val pointB = Point(1f,4f)
      println(s">>> ${pointA.distance(pointB)}")
      def f(p: Point) = {
        println(s"point $p")
      }
      println("<<<<<<<<<<<<")
      val p1 = new Point2D.Float(1f,2f)
      val p2 = new Point2D.Float(1f,2f)
//      f(p1)
      implicitly[Point](p1) match {
        case Point(a,b) => println(s"a: $a, b: $b")
      }
      println(s">>>>>> ${pointA == implicitly[Point](pointB)}")
      println(s":::: ${p1.distance(pointB)}")

      val x: Any = 3
      x match {
        case a:String => println(""" "3"! """)
        case a:Int   => println(s" Three! $a ")
      }
    }
    it("graph test") {
      val g: SimpleWeightedGraph[Point, DefaultWeightedEdge] =
        new SimpleWeightedGraph[Point, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
      val p1 = Point(1,2)
      val p2 = Point(1,2)
      println(s">>>> $g")
      g.addVertex(p1)
      g.addVertex(p2)
      g.addVertex(p1)
      g.addVertex(p2)
      g.addVertex(p2)
      println(s">>>> $g")
    }

    it("graph ") {
      val path1: List[Segment] = List(
        Segment(Point(10.583333f,204.13126f),0),
        Segment(Point(25.135416f,204.13126f),1),
        Segment(Point(25.135416f,213.65625f),1),
        Segment(Point(18.520832f,208.89375f),1),
        Segment(Point(10.583332f,208.89375f),1),
        Segment(Point(10.583332f,208.89375f),4)
      )
      val path2: List[Segment] = List(
        Segment(Point(26.458332f,196.9875f),0),
        Segment(Point(22.489582f,193.41562f),1),
        Segment(Point(26.458332f,189.84375f),1),
        Segment(Point(18.520832f,189.84375f),1),
        Segment(Point(21.166666f,193.41563f),1),
        Segment(Point(18.520832f,196.9875f),1)
      )
      val coordinates: List[List[Segment]] = List(path1,path2)
      val graph = coordinatesToGraph(coordinates: List[List[Segment]])
      graphToCoordinates(graph)
//      println(s">>>>>")
//      println(s"$graph")

      println(s">>>")
//      export(graph)
    }
    it("more") {
      val path1 = List(
        Segment(Point(52.916664f,77.925f),0),
        Segment(Point(79.375f,111.2625f),1),
        Segment(Point(105.83333f,77.925f),1)
      )
      val path2 = List(
        Segment(Point(79.375f,111.2625f),0),
        Segment(Point(105.83333f,139.8375f),1),
        Segment(Point(52.916664f,139.8375f),1),
        Segment(Point(52.916664f,139.8375f),4)
      )
      val coordinates: List[List[Segment]] = List(path1, path2)
      val graph = coordinatesToGraph(coordinates: List[List[Segment]])
      graphToCoordinates(graph)
    }
  }
}
