package net.crinklejoint


import java.util.Comparator
import net.crinklejoint
import org.jgrapht.Graph
import org.jgrapht.alg.connectivity.ConnectivityInspector
import org.jgrapht.event._
import org.jgrapht.graph.{DefaultWeightedEdge, _}
import org.jgrapht.io.{ComponentNameProvider, DOTExporter}
import org.jgrapht.traverse.BreadthFirstIterator
import scala.collection.JavaConversions._
import scala.collection.mutable.{HashMap, ListBuffer, Map, Queue}

//import org.jgrapht._
//import org.jgrapht.graph._
//import org.jgrapht.traverse._
//import java.io._
//import java.net._
//import java.util._
import Protocol._

trait GraphD {

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

  def graphToCoordinates(graph: SimpleWeightedGraph[Point, DefaultWeightedEdge]) = {
    val subVertices = new ConnectivityInspector(graph).connectedSets
    val subGraphs = subVertices.iterator.toList map {
      case vertSet: java.util.Set[Point] => new AsSubgraph[Point, DefaultWeightedEdge](graph, vertSet)
    }
    subGraphs map { subGraph: Graph[Point, DefaultWeightedEdge] =>
      val edges = subGraph.edgeSet
      val segments = new ListBuffer[Segment]
      for (edge: DefaultWeightedEdge <- edges.toList) {
        (subGraph.getEdgeSource(edge), subGraph.getEdgeTarget(edge)) match {
          case (p1, p2) =>
            segments.append(Segment(p1, 0))
            segments.append(Segment(p2, 1))
        }
      }

      segments.toList
    }
  }

  def graph2coordinates(graph: SimpleWeightedGraph[Point, DefaultWeightedEdge]) = {
    val subVertices = new ConnectivityInspector(graph).connectedSets
    val subGraphs = subVertices.iterator.toList map {
      case vertSet: java.util.Set[Point] => new AsSubgraph[Point, DefaultWeightedEdge](graph, vertSet)
    }

    subGraphs flatMap { subGraph: Graph[Point, DefaultWeightedEdge] =>
      val points = subGraph.vertexSet.toList

      val bfIterator = new BreadthFirstIterator(subGraph, points.head)
      val vertexes = graph.vertexSet.toList

      val map: Map[Point, Point] = new HashMap()
      while(bfIterator.hasNext()) {
        val node   = bfIterator.next()
        val parent = bfIterator.getParent(node)
        map.put(node, parent)
      }
      val verts = subGraph.vertexSet.toList
      val coords = (verts map { vert =>
        (vert, bfIterator.getDepth(vert))
      }).sortWith({(p1, p2) =>
        (p1, p2) match {
          case ((_, dp1), (_, dp2)) => dp1 > dp2
        }
      })

      val visited  = new ListBuffer[Point]
      val segments = new ListBuffer[List[Segment]]
      for ((point, _) <- coords) {
        var start = true
        val queue = new Queue[Segment]
        var parent: Point = point
        if(parent != null && !visited.contains(parent)) {
          while (parent != null && !visited.contains(parent)) {
            if (start) {
              queue.enqueue(Segment(parent, 0))
              visited.append(parent)
              start = false
            } else {
              queue.enqueue(Segment(parent, 1))
              visited.append(parent)
            }
            parent = map(parent)
          }
          queue.enqueue(Segment(parent, 1))
          segments.append(queue.toList)
        }
      }
      segments.toList
    }
  }










  def joint(ls: List[(Point, Point)]): List[List[Segment]] = {
    val a = ls.head
    val b = ls.tail.head
    ls match {
      case (a1, null) :: (a2, parent) :: Nil if a1 == parent =>
        List(Segment(a1, 0) :: Segment(a2, 1) :: Nil)
      case (a1, _) :: (a2, parent) :: Nil if a1 == parent    =>
        List(Segment(a1, 1) :: Segment(a2, 1) :: Nil)
      case (a1, _) :: (a2, parent) :: Nil if a1 != parent    =>
        List(Segment(a1, 1)) :: List(Segment(a2, 1) :: Nil)

      case (a1, null) :: (a2, parent) :: tail if a1 == parent =>
        val remaining = joint((a2, parent) :: tail)
        val here      = Segment(a1, 0) :: Segment(a2, 1) :: remaining.head
        here :: remaining.tail
      case (a1, null) :: (a2, parent) :: tail if a1 != parent =>
        val remaining = joint((a2, null) :: tail) //modify p2 to be parentless
        val here      = List(Segment(a1, 0))
        here :: remaining

      case (a1, _) :: (a2, parent) :: tail if a1 == parent =>
        val remaining = joint((a2, parent) :: tail)
        val here      = Segment(a1, 1) :: Segment(a2, 1) :: remaining.head
        here :: remaining.tail
      case (a1, _) :: (a2, parent) :: tail if a1 != parent =>
        val remaining = joint((a2, null) :: tail)
        val here      = List(Segment(a1, 1))
        here :: remaining
    }
  }





  import java.io.StringWriter



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
