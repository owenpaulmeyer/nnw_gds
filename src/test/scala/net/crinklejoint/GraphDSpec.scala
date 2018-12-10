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

      val result = List(
        (Point(105.83333f,   77.925f),  null),
        (Point( 79.375f,    111.2625f), Point(105.83333f, 77.925f)),
        (Point( 52.916664f,  77.925f),
                                        Point(79.375f,   111.2625f)),
        (Point(105.83333f,  139.8375f), Point(79.375f,   111.2625f)),
        (Point( 52.916664f, 139.8375f), Point(79.375f,   111.2625f))
      )
      def joint(ls: List[(Point, Point)], close: Point): List[List[Segment]] = ls match {
        case (a1, null) :: (a2, b2) :: Nil              =>
          List(Segment(a1, 0) :: Segment(a2, 1) :: Nil)

        case (a1, null) :: (a2, b2) :: tail if a1 == b2 =>
          val remaining: List[List[Segment]] = joint(tail, close)
          val here: List[Segment] = Segment(a1, 0) :: Segment(a2, 1) :: remaining.head
          here :: remaining.tail

        case (a1, b1)   :: (a2, b2) :: tail if a1 == b2 =>
          val remaining: List[List[Segment]] = joint(tail, close)
          val here: List[Segment] = Segment(a1, 1) :: Segment(a2, 1) :: remaining.head
          here :: remaining.tail

        case (a1, b1) :: (a2, b2) :: tail               =>
          val remaining: List[List[Segment]] = joint(tail, a2)
          val here: List[Segment] =  Segment(a2, 0) :: remaining.head
          List(Segment(a1, 1)) :: (here :: remaining.tail)

        case (a, b) :: Nil                =>
          List(Segment(a, 1) :: Segment(b, 1) :: Segment(close, 1) :: Nil)

      }
      val jointed = joint(result, null)

      println(s"jointed: $jointed")

//      val coordinates: List[List[Segment]] = List(path1, path2)
//      val graph = coordinatesToGraph(coordinates: List[List[Segment]])
//      graphToCoordinates(graph)
    }
    it("more2") {
      val path1 = List(
        Segment(Point(52.916664f,77.925f),0),
        Segment(Point(79.375f,111.2625f),1)
      )
      val ls = List(
        (Point(52.916664f,77.925f),null),
        (Point(79.375f,111.2625f),Point(52.916664f,77.925f))
      )


      val coordinates: List[List[Segment]] = List(path1)
      val graph = coordinatesToGraph(coordinates: List[List[Segment]])
      graphToCoordinates(graph)
    }
  }
}
