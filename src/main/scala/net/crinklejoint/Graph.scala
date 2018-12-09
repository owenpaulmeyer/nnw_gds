package net.crinklejoint


import org.jgrapht.graph.{DefaultWeightedEdge, SimpleWeightedGraph}

//import org.jgrapht._
//import org.jgrapht.graph._
//import org.jgrapht.traverse._
//import java.io._
//import java.net._
//import java.util._


class Graph extends Protocol {
//  type Point = Point2D.Float

//  val g = new DefaultDirectedGraph[Float, DefaultEdge](classOf[DefaultEdge])

  val g: SimpleWeightedGraph[Point, DefaultWeightedEdge] =
    new SimpleWeightedGraph[Point, DefaultWeightedEdge](classOf[DefaultWeightedEdge])

  val pa = Point(1f,2f)
  val pb = Point(3f,4f)
  val pc = Point(5f,6f)
  pa.distance(pb)

  // add the vertices
  g.addVertex(pa)
  g.addVertex(pb)
  g.addVertex(pc)

  // add edges to create linking structure
  g.addEdge(pc, pb)
  g.addEdge(pa, pc)
  g.addEdge(pa, pb)
  g.addEdge(pb, pa)

  def coordinatesToGraph(coordinates: List[List[Segment]]): SimpleWeightedGraph[Point, DefaultWeightedEdge] = {
    val graph = new SimpleWeightedGraph[Point, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    for (path <- coordinates) {
      for (edge <- path.sliding(2,1)) {
        edge match {
          case List(Segment(p1, x), Segment(p2, 1)) =>
            graph.addVertex(p1)
            graph.addVertex(p2)
            graph.addEdge(p1, p2)
          case List(Segment(p1, 1), Segment(p2, 4)) =>
            graph.addVertex(p1)
            graph.addEdge(p1, p2)
          case _ => throw new RuntimeException("unexpected sequence of segments")
        }
      }
    }

    graph
  }
  def graphToCoordinates(graph: SimpleWeightedGraph[Point, DefaultWeightedEdge]): List[List[Segment]] = {
    val edges = graph.edgeSet()

    
    ???
  }
}
object Graph extends Graph
