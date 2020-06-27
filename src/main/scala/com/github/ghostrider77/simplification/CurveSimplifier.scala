package com.github.ghostrider77.simplification

import scala.annotation.tailrec
import scala.collection.mutable.{PriorityQueue => Heap}

final case class Point(x: Double, y: Double)

class CurveSimplifier(points: Vector[Point]) {
  import CurveSimplifier.{perpendicularDistanceSimplification,
                          radialDistanceSimplification,
                          modifiedDPSimplification,
                          dPSimplification}

  private val nrPoints: Int = points.length
  private val isTrivial: Boolean = nrPoints <= 2

  private def edgeCase(n: Int): Option[Vector[Point]] = {
    if (n <= 0 || points.isEmpty) Some(Vector())
    else if (n == 1) Some(Vector(points(0)))
    else if (n == 2 && nrPoints >= 2) Some(Vector(points(0), points(nrPoints - 1)))
    else if (n >= nrPoints) Some(points)
    else None
  }

  def modifiedDouglasPeucker(n: Int): Vector[Point] = edgeCase(n) match {
    case None => modifiedDPSimplification(points, nrPoints, n)
    case Some(result) => result
  }

  def douglasPeucker(epsilon: Double): Vector[Point] =
    if (isTrivial) points else dPSimplification(points, epsilon)

  def radialDistance(epsilon: Double): Vector[Point] = radialDistanceSimplification(points, nrPoints, epsilon)

  def perpendicularDistance(epsilon: Double): Vector[Point] =
    if (isTrivial) points else perpendicularDistanceSimplification(points, nrPoints, epsilon)
}

object CurveSimplifier {
  private type Index = Int

  private def radialDistanceSimplification(points: Vector[Point], nrPoints: Int, epsilon: Double): Vector[Point] = {
    @tailrec
    def collectKeyPoints(keyPoints: List[Point], currentKeyPoint: Point, ix: Index): Vector[Point] = {
      val point: Point = points(ix)
      if (ix == nrPoints - 1) (point :: keyPoints).toVector.reverse
      else {
        val distance: Double = euclideanDistance(currentKeyPoint, point)
        if (distance >= epsilon) collectKeyPoints(point :: keyPoints, point, ix + 1)
        else collectKeyPoints(keyPoints, currentKeyPoint, ix + 1)
      }
    }

    if (nrPoints < 2) points
    else {
      val initialKeyPoint: Point = points(0)
      collectKeyPoints(List(initialKeyPoint), initialKeyPoint, ix = 1)
    }
  }

  private def perpendicularDistanceSimplification(points: Vector[Point],
                                                  nrPoints: Int,
                                                  epsilon: Double): Vector[Point] = {
    require(nrPoints >= 2)

    @tailrec
    def collectKeyPoints(keyPoints: List[Point], leftEndPoint: Point, ix: Index): Vector[Point] = {
      if (ix >= nrPoints - 1) {
        val lastPoint: Point = points(nrPoints - 1)
        (lastPoint :: keyPoints).reverse.toVector
      }
      else {
        val point: Point = points(ix)
        val rightEndpoint: Point = points(ix + 1)
        val distance: Double = distanceFromSegment(point, leftEndPoint, rightEndpoint)
        if (distance >= epsilon) collectKeyPoints(point :: keyPoints, point, ix + 1)
        else collectKeyPoints(rightEndpoint :: keyPoints, rightEndpoint, ix + 2)
      }
    }

    val initialKeyPoint: Point = points(0)
    collectKeyPoints(List(initialKeyPoint), initialKeyPoint, ix = 1)
  }

  private def dPSimplification(points: Vector[Point], epsilon: Double): Vector[Point] = {
    ???
  }

  private def modifiedDPSimplification(points: Vector[Point], nrPoints: Int, n: Int): Vector[Point] = {
    require(n >= 2)
    val heap: Heap[(Double, Index)] = Heap((Double.PositiveInfinity, 0), (Double.PositiveInfinity, nrPoints - 1))

    @tailrec
    def loop(stack: List[(Index, Index)]): Unit = stack match {
      case Nil => ()
      case (startIx, endIx) :: rest if endIx > startIx + 1 =>
        val (maxDistance, maxIndex): (Double, Index) = calcMaximalDistanceInSegment(points, startIx, endIx)
        heap.enqueue((maxDistance, maxIndex))
        loop((maxIndex, endIx) :: (startIx, maxIndex) :: rest)
      case _ :: rest => loop(rest)
    }

    loop(List((0, nrPoints - 1)))
    val indices: Vector[Index] = extractMaxDistanceIndices(heap, n)
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

  private def distanceFromSegment(p: Point, q1: Point, q2: Point): Double = {
    if (q1.x == q2.x) math.abs(p.x - q1.x)
    else {
      val slope: Double =  (q2.y - q1.y) / (q2.x - q1.x)
      val intercept: Double = q1.y - slope * q1.x
      math.abs(slope * p.x - p.y + intercept) / math.sqrt(math.pow(slope, 2) + 1)
    }
  }

  private def euclideanDistance(p: Point, q: Point): Double = math.hypot(p.x - q.x, p.y - q.y)
}
