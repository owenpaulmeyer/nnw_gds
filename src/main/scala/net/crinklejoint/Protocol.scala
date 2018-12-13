package net.crinklejoint

import java.awt.geom.Point2D

trait Protocol {
  case class Point(x: scala.Float, y: scala.Float)
  implicit def convertToJava(p: Point) =
    new Point2D.Float(p.x.asInstanceOf[java.lang.Float], p.y.asInstanceOf[java.lang.Float])
  implicit def convertToScala(p: Point2D.Float) = Point(p.x, p.y)

  case class Segment(point: Point, directive: Int)
  // Segment Directives:
  val SEG_MOVETO = 0
  val SEG_LINETO = 1
  val SEG_CLOSE  = 4
}

object Protocol extends Protocol
