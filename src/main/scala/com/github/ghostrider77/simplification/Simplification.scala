package com.github.ghostrider77.simplification

import scala.annotation.tailrec
import scala.collection.mutable.{PriorityQueue => Heap}

object Simplification {
  private type Index = Int

  final case class Point(x: Double, y: Double)

  def simplify(points: Vector[Point], size: Int): Vector[Point] = {
    val length: Int = points.length
    if (size <= 0 || points.isEmpty) Vector()
    else if (size == 1) Vector(points.head)
    else if (size == 2 && length >= 2) Vector(points.head, points.last)
    else if (size >= length) points
    else calcLineRepresentation(points, length, size)
  }

  private def distanceFromSegment(p: Point, q1: Point, q2: Point): Double = {
    if (q1.x == q2.x) math.abs(p.x - q1.x)
    else {
      val slope: Double =  (q2.y - q1.y) / (q2.x - q1.x)
      val intercept: Double = q1.y - slope * q1.x
      math.abs(slope * p.x - p.y + intercept) / math.sqrt(math.pow(slope, 2) + 1)
    }
  }

  private def calcLineRepresentation(points: Vector[Point], length: Int, size: Int): Vector[Point] = {
    val heap: Heap[(Double, Index)] = Heap((Double.PositiveInfinity, 0), (Double.PositiveInfinity, length - 1))

    @tailrec
    def loop(stack: List[(Index, Index)]): Unit = stack match {
      case Nil => ()
      case (startIx, endIx) :: rest if endIx > startIx + 1 =>
        val (maxDistance, maxIndex): (Double, Index) = calcMaximalDistanceInSegment(points, startIx, endIx)
        heap.enqueue((maxDistance, maxIndex))
        loop((maxIndex, endIx) :: (startIx, maxIndex) :: rest)
      case _ :: rest => loop(rest)
    }

    loop(List((0, length - 1)))
    val indices: Vector[Index] = extractMaxDistanceIndices(heap, size)
    indices.map(points)
  }

  private def calcMaximalDistanceInSegment(points: Vector[Point], startIx: Index, endIx: Index): (Double, Index) = {
    val leftEndPoint: Point = points(startIx)
    val rightEndPoint: Point = points(endIx)

    @tailrec
    def visitPointsInSegment(maxDistance: Double, maxIndex: Index, ix: Int): (Double, Index) = {
      if (ix >= endIx) (maxDistance, maxIndex)
      else {
        val point: Point = points(ix)
        val distance: Double = distanceFromSegment(point, leftEndPoint, rightEndPoint)
        if (distance > maxDistance) visitPointsInSegment(distance, ix, ix + 1)
        else visitPointsInSegment(maxDistance, maxIndex, ix + 1)
      }
    }

    visitPointsInSegment(-1.0, startIx, startIx + 1)
  }

  private def extractMaxDistanceIndices(heap: Heap[(Double, Index)], size: Int): Vector[Index] = {
    require(heap.size >= size)
    @tailrec
    def nLargest(acc: List[Index], n: Int): Vector[Index] = {
      if (n >= size) acc.sorted.toVector
      else {
        val (_, ix): (Double, Index) = heap.dequeue()
        nLargest(ix :: acc, n + 1)
      }
    }
    nLargest(Nil, 0)
  }
}
