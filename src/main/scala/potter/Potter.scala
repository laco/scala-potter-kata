package potter

import scala.annotation.tailrec
import scala.collection.mutable

object Potter {
  type Series = Vector[Int]

  val oneBookPrice = 8.0
  def discountFor(uniqItemCount: Int): Double = uniqItemCount match {
    case 0 => 1.0
    case 1 => 1.0
    case 2 => 0.95
    case 3 => 0.90
    case 4 => 0.80
    case 5 => 0.75
  }

  def sumPrice(bookListList: List[Series]) =
    (for (bl <- bookListList) yield price(bl.toList)).toList.sum

  val possibleSeriesCache: mutable.Map[(Int, List[Series]), Set[List[Series]]] =
    mutable.Map()

  def possibleSeries(head: Int,
                     currentSeries: List[Series]): Set[List[Series]] = {

    @tailrec
    def acc(pastSeries: List[Series],
            current: Series,
            futureSeries: List[Series],
            result: Set[List[Series]] = Set()): Set[List[Series]] = {
      if (current.contains(head)) {
        futureSeries match {
          case Nil => result
          case fhead :: ftail =>
            acc(current :: pastSeries, fhead, ftail, result)
        }
      } else {
        val newResult = result + ((head +: current) :: pastSeries ++ futureSeries)
        futureSeries match {
          case Nil => newResult
          case fhead :: ftail =>
            acc(current :: pastSeries, fhead, ftail, newResult)
        }
      }
    }
    if (possibleSeriesCache.isDefinedAt((head, currentSeries))) {
      possibleSeriesCache((head, currentSeries))
    } else {
      val result = currentSeries match {
        case Nil => Set(List(Vector(head)))
        case chead :: ctail =>
          acc(List(), chead, ctail, Set(Vector(head) :: currentSeries))
      }
      possibleSeriesCache((head, currentSeries)) = result
      result
    }
  }

  def genAllPossibleSeries(bookList: List[Int]): Set[List[Series]] = {

    bookList match {
      case Nil => Set()
      case head :: Nil => possibleSeries(head, List())
      case head :: tail => {
        var result: Set[List[Series]] = Set()
        for {
          j <- genAllPossibleSeries(tail)
          s <- possibleSeries(head, j)
        } yield {
          result = result + s
        }
        result
      }
    }
  }

  def isAllUniq(bookList: List[Int]): Boolean =
    bookList.length == bookList.toSet.toList.length

  def price(bookList: List[Int]): Double = {
    if (isAllUniq(bookList)) {
      oneBookPrice * bookList.length * discountFor(bookList.length)
    } else {
      (for {
        uniqBookLists <- genAllPossibleSeries(bookList)
      } yield {
        sumPrice(uniqBookLists)
      }).toList.min
    }
  }

}
