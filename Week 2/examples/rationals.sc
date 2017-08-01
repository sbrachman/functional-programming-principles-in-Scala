object rationals {

  val x = new Rational(1,2)
  val y = new Rational(1,3)
  val z = new Rational(3,2)

  x * x + y * y

  x - y

  y < z

  y max x


  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be nonzero")

    def this(x: Int) = this(x, 1)

    val numer = x / gcd(x, y)
    val denom = y / gcd(x, y)

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)

    def < (that: Rational) =
      numer * that.denom < that.numer * denom

    def max(that: Rational) =
      if (this < that) that else this

    def + (that: Rational) =
      new Rational(numer * that.denom + that.numer * denom,
        denom * that.denom)

    def - (that: Rational) = this + -that

    def * (that: Rational) =
      new Rational(numer * that.numer, denom * that.denom)

    def / (that: Rational) =
      new Rational(numer * that.denom, denom * that.numer)

    def unary_- : Rational = new Rational(-numer, denom)

    override def toString = numer + "/" + denom

  }
}




