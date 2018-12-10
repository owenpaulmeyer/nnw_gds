package net.crinklejoint


import org.jgrapht.Graph
import org.jgrapht.alg.connectivity.ConnectivityInspector
import org.jgrapht.graph.{AsSubgraph, DefaultWeightedEdge, SimpleWeightedGraph}
import org.jgrapht.io.{ComponentNameProvider, DOTExporter}
import org.jgrapht.traverse.BreadthFirstIterator
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

//import org.jgrapht._
//import org.jgrapht.graph._
//import org.jgrapht.traverse._
//import java.io._
//import java.net._
//import java.util._


trait GraphD extends Protocol {
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
      val head = path.head
      for (edge <- path.sliding(2,1)) {
        edge match {
          case List(Segment(p1, x), Segment(p2, 1)) =>
            graph.addVertex(p1)
            graph.addVertex(p2)
            graph.addEdge(p1, p2)
          case List(Segment(p1, 1), Segment(p2, 4)) if p1 == p2  =>
            graph.addVertex(p1)
            graph.addEdge(p1, head.point)
          case _ => throw new RuntimeException("unexpected sequence of segments")
        }
      }
    }

    graph
  }
  def graphToCoordinates(graph: SimpleWeightedGraph[Point, DefaultWeightedEdge]): List[List[Segment]] = {
    val subVertices = new ConnectivityInspector(graph).connectedSets
    val subGraphs = subVertices.iterator.toList map { case vertSet: java.util.Set[Point] => new AsSubgraph[Point, DefaultWeightedEdge](graph, vertSet) }

//    for (subGraph: Graph[Point, DefaultWeightedEdge]  <- subGraphs)
    subGraphs flatMap { subGraph: Graph[Point, DefaultWeightedEdge] =>
      val points = subGraph.vertexSet.toList
      val pathCaps = points.filter{
        point =>
          val edges = subGraph.edgesOf(point)
          edges.size == 1
      }
      val start = pathCaps.headOption.getOrElse(points.head)

      val bfIterator = new BreadthFirstIterator(subGraph, start)

      val buffer = new ListBuffer[(Point, Point)]
      while(bfIterator.hasNext()) {
        val node = bfIterator.next()
        val parent = bfIterator.getParent(node)
        buffer.append((node, parent))
      }
      println(s"$buffer")
      List()
    }


    List.empty
  }


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

  import java.io.StringWriter
// use helper classes to define how vertices should be rendered,// use helper classes to define how vertices should be rendered,

  // adhering to the DOT language restrictions



  def export(hrefGraph: Graph[Point, DefaultWeightedEdge]) {
    val vertexIdProvider = new ComponentNameProvider[Point] {
      def getName(point: Point): String = point.toString
          .replace('.', 'p')
          .replace(',', 'C')
          .replace('(', 'L')
          .replace(')', 'R')
    }
    val vertexLabelProvider = new ComponentNameProvider[Point] {
      def getName(point: Point): String = point.toString
          .replace('.', '8')
          .replace(',', 'c')
          .replace('(', 'L')
          .replace(')', 'R')
    }
    val edgeLabelProvider = new ComponentNameProvider[DefaultWeightedEdge] {
      def getName(edge: DefaultWeightedEdge): String = edge.toString
    }
    val exporter = new DOTExporter(vertexIdProvider, vertexLabelProvider, edgeLabelProvider)
    val writer = new StringWriter
    exporter.exportGraph(hrefGraph, writer)
    println(writer.toString)
  }



}
object GraphD extends GraphD
