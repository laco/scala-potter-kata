package potter

/**
  * Created by landrasi on 2016.08.25..
  */
import Potter._
import org.scalatest.{FreeSpec, Matchers}

class PotterPricingSuite extends FreeSpec with Matchers {
  /*  "possibleSeries" in {
    possibleSeries(0, List(Vector(0))) should equal(
      Set(List(Vector(0), Vector(0))))
    possibleSeries(0, List(Vector(1))) should equal(
      Set(List(Vector(0), Vector(1)), List(Vector(0, 1))))
    possibleSeries(0, List(Vector(0), Vector(1))) should equal(
      Set(List(Vector(0), Vector(0), Vector(1)),
          List(Vector(0), Vector(0, 1))))

  }
   */

  "price" - {
    "empty basket" in {
      price(List()) should equal(0.0)
    }
    "single book in the basket" in {
      for {
        i <- 0 to 4
      } yield {
        price(List(i)) should equal(oneBookPrice)
      }
    }
    "same unique book many-many times " in {
      for {
        i <- 0 to 4
        times <- 1 to 5
      } yield {
        val bookList = (for (k <- 1 to times) yield i).toList
        price(bookList) should equal(oneBookPrice * times)
      }
    }

    "test simple discount with single series" in {
      price((0 to 1).toList) should equal(oneBookPrice * 2 * 0.95)
      price((0 to 2).toList) should equal(oneBookPrice * 3 * 0.90)
      price((0 to 3).toList) should equal(oneBookPrice * 4 * 0.80)
      price((0 to 4).toList) should equal(oneBookPrice * 5 * 0.75)
    }

    "test two books on series one is not" in {
      price(List(0, 0, 1)) should equal(
        oneBookPrice + (oneBookPrice * 2 * 0.95))
    }
    "test 2x2 books on series" in {
      price(List(0, 1, 0, 1)) should equal(2 * (2 * oneBookPrice * 0.95))
    }

    "more discounts" in {
      price(List(0, 0, 1, 2, 2, 3)) should equal(
        (8.0 * 4 * 0.8) + (8.0 * 2 * 0.95))
      price(List(0, 1, 1, 2, 3, 4)) should equal(8.0 + (8.0 * 5 * 0.75))
    }

    "test edge cases " in {
      price(List(0, 0, 1, 1, 2, 2, 3, 4)) should equal(2 * (8.0 * 4 * 0.8))

      /*      price(
        List(0,
          0,
          0,
          0,
          0,
          1,
          1,
          1,
          1,
          1,
          2,
          2,
          2,
          2,
          3,
          3,
          3,
          3,
          3,
          4,
          4,
          4,
          4)) should equal(3 * (8 * 5 * 0.75) + 2 * (8 * 4 * 0.8))

     // This one will fail with out of memory after 12 minutes... :D
     */
    }
  }
}
