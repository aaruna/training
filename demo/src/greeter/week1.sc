val x = 1
def increase(i: Int) = i + 1
increase(x)


def loop: Int = loop

def constOne(x: => Int, y: => Int) = 2

1 + 2


def abs(x: Double) = if (x > 0) x else -x

def sqrt(x: Double) = {
  def sqrIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else
      sqrIter(improve(guess))

  def isGoodEnough(guess: Double): Boolean = abs(guess * guess - x) / x < 0.001
  def improve(guess: Double): Double = (guess + x / guess) / 2

  sqrIter(1)
}

sqrt(2)
sqrt(4)

sqrt(0.001)
sqrt(1e-6)
sqrt(1e60)