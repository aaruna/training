case class Polynomial(_terms: Map[Int, Double]) {
  val terms = _terms withDefaultValue 0.0

  def this(pa: (Int, Double)*) = {
      this(pa.toMap)
  }

  def unary_-(): Polynomial = new Polynomial(terms.mapValues (_ * -1))

  def + (other: Polynomial): Polynomial = {
    def newTerms = other.terms.foldLeft(terms) { (t: Map[Int, Double], a: (Int, Double)) =>
      val (deg, coeff) = a
      t + (deg -> (t(deg) + coeff))
    }
    new Polynomial(newTerms)
  }

  def - (other: Polynomial): Polynomial = this + (-other)

  override def toString: String = {
    (for {
      (deg, coeff) <- terms.toList.sorted.reverse
    } yield s"$coeff x^$deg" ) mkString " + "
  }
}

new Polynomial(3 -> 9, 2 -> 5) + new Polynomial(4 -> 9, 2 -> 5)
new Polynomial(3 -> 9, 2 -> 5) - new Polynomial(4 -> 9, 2 -> 5)