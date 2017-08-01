object fixedPoints {

  val tolerance = 0.0001

  def abs(d: Double): Double =
    if (d < 0) -d
    else d

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y)/ x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoint(x => 1 + x/2)(1)

  def sqrt(x: Double) =
    fixedPoint(averageDamp(y => x / y))(1)

  sqrt(9)


  def averageDamp(f: Double => Double)(x: Double) =
    (x + f(x)) / 2


}