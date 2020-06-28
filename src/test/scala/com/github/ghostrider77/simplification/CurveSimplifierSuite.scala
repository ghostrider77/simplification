package com.github.ghostrider77.simplification

import org.scalatest.{FreeSpec, Inspectors, Matchers, PrivateMethodTester}
import scala.collection.mutable.{PriorityQueue => Heap}

class CurveSimplifierSuite extends FreeSpec with Matchers with Inspectors with PrivateMethodTester {
  object Curves {
    val tolerance: Double = 1e-8
    val points: Vector[Point] =
      Vector(
        Point(0, 0),
        Point(1, 1),
        Point(0, 2),
        Point(2, 2),
        Point(3, 0),
        Point(4, 0),
        Point(3, 1),
        Point(4, 1),
        Point(5, 0)
      )
  }

  "CommonFunctions" - {
    import Curves.tolerance

    "DistanceFromSegment" - {
      "Should calculate the distance of a point from a segment given by its endpoints when" - {
        val origin: Point = Point(0, 0)
        val distanceFromSegment = PrivateMethod[Double](Symbol("distanceFromSegment"))

        "distance from horizontal line is calculated" in {
          val p: Point = Point(5, 2)
          val q: Point = Point(6, 0)
          CurveSimplifier invokePrivate distanceFromSegment(p, origin, q) should be(2.0 +- tolerance)
        }

        "point is outside of the segment" in {
          val p: Point = Point(10, 2)
          val q: Point = Point(6, 0)
          CurveSimplifier invokePrivate distanceFromSegment(p, origin, q) should be(2.0 +- tolerance)
        }

        "the point is in general position with respect to the segment" in {
          val d1: Double = CurveSimplifier invokePrivate distanceFromSegment(Point(3, 3), origin, Point(4, 2))
          val d2: Double = CurveSimplifier invokePrivate distanceFromSegment(Point(3, 8), origin, Point(4, 2))
          val d3: Double = CurveSimplifier invokePrivate distanceFromSegment(Point(1, 3), Point(0, 2), Point(2, 0))

          d1 shouldBe ((1 / 5.0) * math.sqrt(45) +- tolerance)
          d2 shouldBe ((1 / 5.0) * math.sqrt(845) +- tolerance)
          d3 shouldBe (math.sqrt(2) +- tolerance)
        }
      }
    }

    "calcMaximalDistanceInSegment" - {
      import Curves.tolerance
      val calcMaximalDistanceInSegment = PrivateMethod[(Double, Int)](Symbol("calcMaximalDistanceInSegment"))

      "Should calculate the index of the point that has the largest distance from a segment" in {
        val points: Vector[Point] =
          Vector(Point(0, 0), Point(1, 0), Point(2, 1), Point(3, -1), Point(4, 2), Point(5, 0), Point(6, 1))
        val startIx: Int = 1
        val endIx: Int = 5
        val (maxDistance, maxIndex): (Double, Int) =
          CurveSimplifier invokePrivate calcMaximalDistanceInSegment(points, startIx, endIx)
        maxIndex shouldEqual 4
        maxDistance should be(2.0 +- tolerance)
      }
    }

    "ExtractMaxDistanceIndices" - {
      val extractMaxDistanceIndices = PrivateMethod[Vector[Int]](Symbol("extractMaxDistanceIndices"))

      "Should retrieve the indices of the top n points with the largest pre-calculated distances" in {
        val heap: Heap[(Double, Int)] =
          Heap((10.0, 0), (8.0, 1), (0.0, 2), (1.0, 3), (0.5, 4), (11.0, 5), (7.0, 6), (0.0, 7), (15.0, 8), (2.0, 9))

        val indices: Vector[Int] = CurveSimplifier invokePrivate extractMaxDistanceIndices(heap, 4)
        indices should contain theSameElementsAs Vector(8, 5, 0, 1)
      }

      "Should throw an exception if the number of points is less than the requested size of the simplified curve" in {
        val heap: Heap[(Double, Int)] = Heap((1.0, 0))
        an[IllegalArgumentException] should be thrownBy
          (CurveSimplifier invokePrivate extractMaxDistanceIndices(heap, 2))
      }
    }
  }

  "RadialDistanceSimplification" - {
    "should retrieve the orignal points when there are at most two points only" - {
      "test case 1" - {
        val points: Vector[Point] = Vector()
        val simplificator = new CurveSimplifier(points)
        simplificator.radialDistance(epsilon = 0.1) shouldBe empty
      }

      "test case 2" - {
        val points: Vector[Point] = Vector(Point(0, 0))
        val simplificator = new CurveSimplifier(points)
        simplificator.radialDistance(epsilon = 0.1) shouldEqual points
      }

      "test case 3" - {
        val points: Vector[Point] = Vector(Point(10, 10), Point(0, 0))
        val simplificator = new CurveSimplifier(points)
        simplificator.radialDistance(epsilon = 0.1) shouldEqual points
      }
    }

    "should remove consecutive vertices that fall within a specified distance tolerance from the last keypoint" - {
      "test case 1" in {
        val points: Vector[Point] =
          Vector(Point(0, 0), Point(1, 1), Point(2, 1), Point(3, 1), Point(4, 3), Point(5, 1), Point(6, 1))
        val simplificator = new CurveSimplifier(points)
        val result: Vector[Point] = simplificator.radialDistance(epsilon = 1.5)
        result shouldEqual Vector(Point(0, 0), Point(2, 1), Point(4, 3), Point(5, 1), Point(6, 1))
      }

      "test case 2" in {
        val points: Vector[Point] =
          Vector(Point(0, 0), Point(1, 0), Point(2, 0))
        val simplificator = new CurveSimplifier(points)
        val result: Vector[Point] = simplificator.radialDistance(epsilon = 1.5)
        result shouldEqual Vector(Point(0, 0), Point(2, 0))
      }

      "test case 3" in {
        val points: Vector[Point] =
          Vector(Point(0, 0), Point(1, 0), Point(2, 0))
        val simplificator = new CurveSimplifier(points)
        val result: Vector[Point] = simplificator.radialDistance(epsilon = 0.5)
        result shouldEqual Vector(Point(0, 0), Point(1, 0), Point(2, 0))
      }
    }

    "should remove consecutive vertices for two dimensional curves" - {
      import Curves.points
      val simplificator = new CurveSimplifier(points)

      "test case 1" in {
        val result: Vector[Point] = simplificator.radialDistance(epsilon = 1.5)
        result shouldEqual Vector(Point(0, 0), Point(0, 2), Point(2, 2), Point(3, 0), Point(5, 0))
      }

      "test case 2" in {
        val result: Vector[Point] = simplificator.radialDistance(epsilon = 1.2)
        result shouldEqual
          Vector(Point(0, 0), Point(1, 1), Point(0, 2), Point(2, 2), Point(3, 0), Point(4, 1), Point(5, 0))
      }
    }
  }

  "PerpendicularDistanceSimplification" - {
    "should retrieve the orignal points when there are at most two points only" - {
      "test case 1" - {
        val points: Vector[Point] = Vector()
        val simplificator = new CurveSimplifier(points)
        simplificator.perpendicularDistance(epsilon = 0.1) shouldBe empty
      }

      "test case 2" - {
        val points: Vector[Point] = Vector(Point(0, 0))
        val simplificator = new CurveSimplifier(points)
        simplificator.perpendicularDistance(epsilon = 0.1) shouldEqual points
      }

      "test case 3" - {
        val points: Vector[Point] = Vector(Point(10, 10), Point(0, 0))
        val simplificator = new CurveSimplifier(points)
        simplificator.perpendicularDistance(epsilon = 0.1) shouldEqual points
      }
    }

    "should remove the middle point of a segment if it falls within a specified distance tolerance" - {
      "test case 1" in {
        val points: Vector[Point] = Vector(Point(0, 0), Point(1, 1), Point(2, 0), Point(3, 0))
        val simplificator = new CurveSimplifier(points)
        val result: Vector[Point] = simplificator.perpendicularDistance(epsilon = 0.1)
        result shouldEqual Vector(Point(0, 0), Point(1, 1), Point(2, 0), Point(3, 0))
      }

      "test case 2" in {
        val points: Vector[Point] = Vector(Point(0, 0), Point(1, 1), Point(2, 0), Point(3, 0))
        val simplificator = new CurveSimplifier(points)
        val result: Vector[Point] = simplificator.perpendicularDistance(epsilon = 0.9)
        result shouldEqual Vector(Point(0, 0), Point(1, 1), Point(3, 0))
      }

      "test case 3" in {
        val points: Vector[Point] = Vector(Point(0, 0), Point(1, 1), Point(2, 0), Point(3, 0))
        val simplificator = new CurveSimplifier(points)
        val result: Vector[Point] = simplificator.perpendicularDistance(epsilon = 1.5)
        result shouldEqual Vector(Point(0, 0), Point(2, 0), Point(3, 0))
      }
    }

    "should remove consecutive vertices for two dimensional curves based on perpendicular distance from segment" - {
      val simplificator = new CurveSimplifier(Curves.points)

      "test case 1" in {
        val result: Vector[Point] = simplificator.perpendicularDistance(epsilon = 1.5)
        result shouldEqual Vector(Point(0, 0), Point(0, 2), Point(3, 0), Point(3, 1), Point(5, 0))
      }

      "test case 2" in {
        val result: Vector[Point] = simplificator.perpendicularDistance(epsilon = 1.2)
        result shouldEqual Vector(Point(0, 0), Point(0, 2), Point(3, 0), Point(3, 1), Point(5, 0))
      }

      "test case 3" in {
        val result: Vector[Point] = simplificator.perpendicularDistance(epsilon = 0.9)
        result shouldEqual
          Vector(Point(0, 0), Point(1, 1), Point(0, 2), Point(2, 2), Point(4, 0), Point(3, 1), Point(5, 0))
      }
    }
  }

  "DouglasPeucker" - {
    "Should keep points that are far enough from the actual segment" - {
      "test case 1" in {
        val points: Vector[Point] = Vector(Point(0, 2), Point(1, 3), Point(2, 0), Point(3, 1), Point(4, 2))
        val simplificator = new CurveSimplifier(points)
        val result: Vector[Point] = simplificator.douglasPeucker(epsilon = 2.1)
        result shouldEqual Vector(Point(0, 2), Point(4, 2))
      }

      "test case 2" in {
        val points: Vector[Point] = Vector(Point(0, 2), Point(1, 3), Point(2, 0), Point(3, 1), Point(4, 2))
        val simplificator = new CurveSimplifier(points)
        val result: Vector[Point] = simplificator.douglasPeucker(epsilon = 1.1)
        result shouldEqual Vector(Point(0, 2), Point(1, 3), Point(2, 0), Point(4, 2))
      }

      "test case 3" in {
        val points: Vector[Point] = Vector(Point(0, 2), Point(1, 3), Point(2, 0), Point(3, 1), Point(4, 2))
        val simplificator = new CurveSimplifier(points)
        val result: Vector[Point] = simplificator.douglasPeucker(epsilon = 1.5)
        result shouldEqual Vector(Point(0, 2), Point(2, 0), Point(4, 2))
      }
    }

    "Should keep points of a two dimensional curve based on perpendicular distance from actual segment" in {
      val simplificator = new CurveSimplifier(Curves.points)
      val result: Vector[Point] = simplificator.douglasPeucker(epsilon = 0.9)
      result shouldEqual Vector(Point(0, 0), Point(1, 1), Point(0, 2), Point(5, 0))
    }
  }

  "ModifiedDouglasPeucker" - {

  }
}
