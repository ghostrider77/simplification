package com.github.ghostrider77.simplification

import org.scalatest.{FreeSpec, Inspectors, Matchers, PrivateMethodTester}
import scala.collection.mutable.{PriorityQueue => Heap}

class SimplificationSuite extends FreeSpec with Matchers with Inspectors with PrivateMethodTester {
  import com.github.ghostrider77.simplification.Simplification.Point

  val tolerance: Double = 1e-8

  "DistanceFromSegment" - {
    "Should calculate the distance of a point from a segment given by its endpoints when" - {
      val origin: Point = Point(0, 0)
      val distanceFromSegment = PrivateMethod[Double](Symbol("distanceFromSegment"))

      "distance from horizontal line is calculated" in {
        val p: Point = Point(5, 2)
        val q: Point = Point(6, 0)
        Simplification invokePrivate distanceFromSegment(p, origin, q) should be (2.0 +- tolerance)
      }

      "point is outside of the segment" in {
        val p: Point = Point(10, 2)
        val q: Point = Point(6, 0)
        Simplification invokePrivate distanceFromSegment(p, origin, q) should be (2.0 +- tolerance)
      }

      "the point is in general position with respect to the segment" in {
        val d1: Double = Simplification invokePrivate distanceFromSegment(Point(3, 3), origin, Point(4, 2))
        val d2: Double = Simplification invokePrivate distanceFromSegment(Point(3, 8), origin, Point(4, 2))
        val d3: Double = Simplification invokePrivate distanceFromSegment(Point(1, 3), Point(0, 2), Point(2, 0))

        d1 shouldBe ((1 / 5.0) * math.sqrt(45) +- tolerance)
        d2 shouldBe ((1 / 5.0) * math.sqrt(845) +- tolerance)
        d3 shouldBe (math.sqrt(2) +- tolerance)
      }
    }
  }

  "ExtractMaxDistanceIndices" - {
    val extractMaxDistanceIndices = PrivateMethod[Vector[Int]](Symbol("extractMaxDistanceIndices"))

    "Should retrieve the indices of the top n points with the largest pre-calculated distances" in {
      val heap: Heap[(Double, Int)] =
        Heap((10.0, 0), (8.0, 1), (0.0, 2), (1.0, 3), (0.5, 4), (11.0, 5), (7.0, 6), (0.0, 7), (15.0, 8), (2.0, 9))

      val indices: Vector[Int] = Simplification invokePrivate extractMaxDistanceIndices(heap, 4)
      indices should contain theSameElementsAs Vector(8, 5, 0, 1)
    }

    "Should throw an exception if the number of points is less than the requested size of the simplified curve" in {
      val heap: Heap[(Double, Int)] = Heap((1.0, 0))
      an[IllegalArgumentException] should be thrownBy (Simplification invokePrivate extractMaxDistanceIndices(heap, 2))
    }
  }

  "calcMaximalDistanceInSegment" - {
    val calcMaximalDistanceInSegment = PrivateMethod[(Double, Int)](Symbol("calcMaximalDistanceInSegment"))

    "Should calculate the index of the point that has the largest distance from a segment" in {
      val points: Vector[Point] =
        Vector(Point(0, 0), Point(1, 0), Point(2, 1), Point(3, -1), Point(4, 2), Point(5, 0), Point(6, 1))
      val startIx: Int = 1
      val endIx: Int = 5
      val (maxDistance, maxIndex): (Double, Int) =
        Simplification invokePrivate calcMaximalDistanceInSegment(points, startIx, endIx)
      maxIndex shouldEqual 4
      maxDistance should be (2.0 +- tolerance)
    }
  }

  "Simplify" - {
    import com.github.ghostrider77.simplification.Simplification.simplify

    "Should handle all edge cases" in {
      simplify(Vector(), -1) shouldBe empty
      simplify(Vector(), 1) shouldBe empty
      simplify(Vector(), 10) shouldBe empty
      simplify(Vector(Point(0, 0)), 10) shouldEqual Vector(Point(0, 0))
      simplify(Vector(Point(0, 0), Point(0, 1), Point(3, 4)), 10) should have length 3
      simplify(Vector(Point(0, 0), Point(0, 1), Point(3, 4)), 1) should have length 1
    }

    "Should simplify a curve" in {
      val points: Vector[Point] = Vector(Point(0, 0), Point(1, 1.5), Point(2, 2), Point(3, 1), Point(4, 0))
      val result: Vector[Point] = simplify(points, 4)
      val expected: Vector[Point] = Vector(Point(0, 0), Point(1, 1.5), Point(2, 2), Point(4, 0))
      forAll (result.zip(expected)) {
        case (Point(a, b), Point(x, y)) =>
          a shouldBe (x +- tolerance)
          b shouldBe (y +- tolerance)
      }
    }
  }
}
