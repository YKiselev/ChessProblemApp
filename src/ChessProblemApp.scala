/**
 * Program to find all distinct layouts for specified chess figures and field dimensions
 *
 * @param figures - list of used figures
 * @param width - field width
 * @param height - field height
 */
class ChessProblemApp(val figures: List[Byte], val width: Int, val height: Int) {

  private val length = width * height
  //private var layouts: Int = 0

  def findLayouts() {
    //combine(0, Nil, figures)
    println("\nTotal distinct layouts: " + combine(0, Nil, figures))
  }

  private def combine(index: Int, field: List[(Byte, Int, Int)], figures: List[Byte]): Int = {

    def placeFigure(figure: Byte, field: List[(Byte, Int, Int)]): Option[(Byte, Int, Int)] = field match {
      case h :: t =>
        val fi = index % width
        val fj = index / width

        if (canEat(fi, fj, figure, h._2, h._3) || canEat(h._2, h._3, h._1, fi, fj))
          None
        else
          placeFigure(figure, t)

      case Nil =>
        Some(figure, index % width, index / width)
    }

    def tryFigure(figure: Byte): Int = {
      figure match {
        case ChessProblemApp.EMPTY =>
          combine(index + 1, field, figures)
        case f =>
          placeFigure(f, field).map(v => combine(index + 1, v :: field, figures diff List(figure))).sum
      }
    }

    if (index >= length) {
      1
    } else {
      val result = figures.distinct.map(figure => tryFigure(figure)).sum
      if (index + figures.size < length)
        result + tryFigure(ChessProblemApp.EMPTY)
      else
        result
    }
  }

  private def canEat(fi: Int, fj: Int, figure: Byte, i: Int, j: Int): Boolean = figure match {
    case ChessProblemApp.BISHOP => bishopCanEat(fi, fj, i, j)
    case ChessProblemApp.KING => kingCanEat(fi, fj, i, j)
    case ChessProblemApp.KNIGHT => knightCanEat(fi, fj, i, j)
    case ChessProblemApp.QUEEN => queenCanEat(fi, fj, i, j)
    case ChessProblemApp.ROOK => rookCanEat(fi, fj, i, j)
    case _ => false
  }

  private def sameDiagonal(i1: Int, j1: Int, i2: Int, j2: Int): Boolean =
    Math.abs(i1 - i2) == Math.abs(j1 - j2)

  private def bishopCanEat(fi: Int, fj: Int, i: Int, j: Int): Boolean =
    (i == fi && j == fj) || sameDiagonal(fi, fj, i, j)

  private def kingCanEat(fi: Int, fj: Int, i: Int, j: Int): Boolean =
    (i >= fi - 1 && i <= fi + 1) && (j >= fj - 1 && j <= fj + 1)

  private def queenCanEat(fi: Int, fj: Int, i: Int, j: Int): Boolean =
    (fi == i) || (fj == j) || sameDiagonal(fi, fj, i, j)

  private def rookCanEat(fi: Int, fj: Int, i: Int, j: Int): Boolean =
    (fi == i) || (fj == j)


  private def knightCanEat(fi: Int, fj: Int, i: Int, j: Int): Boolean = {
    (fi == i && fj == j) || (fi - 2 == i && (fj - 1 == j || fj + 1 == j)) ||
      (fi - 1 == i && (fj - 2 == j || fj + 2 == j)) ||
      (fi + 2 == i && (fj - 1 == j || fj + 1 == j)) ||
      (fi + 1 == i && (fj - 2 == j || fj + 2 == j))
  }
}

object ChessProblemApp {

  final val EMPTY: Byte = 0
  final val KING: Byte = 1
  final val QUEEN: Byte = 2
  final val BISHOP: Byte = 3
  final val ROOK: Byte = 4
  final val KNIGHT: Byte = 5

  def main(args: Array[String]) {
    val t0 = System.nanoTime()
    //new ChessProblemApp(List(KING, ROOK, KING), 3, 3).findLayouts()
    //new ChessProblemApp(List(KNIGHT, ROOK, KNIGHT, ROOK, KNIGHT, KNIGHT), 4, 4).findLayouts()
    new ChessProblemApp(List(KING, KING, QUEEN, BISHOP, ROOK, KNIGHT), 6, 9).findLayouts()
    println("time (s):" + 0.001 * ((System.nanoTime() - t0) / 1000000))
  }

}
