import scala.annotation.tailrec

// not tail recursive style
def factorial(x: Int): Long = if(x == 0) 1 else x * factorial(x - 1)

factorial(5) // ok, but...
// factorial(100000) // java.lang.StackOverflowError too deep recursion

// factorial tail recursive way: very last expr recursion call without any additions
// inner tail recursive function + accumulator trick + call to inner with set accumulator
// 1 for multiplication and 0 for addition <- unit values
def factorialTR(x: Int): Long = {

  @tailrec
  def factIterate(acc: Long, step: Long): Long =
    if (step == 1) acc else {
      factIterate(acc * step, step - 1)
    }

  factIterate(1, x) // początkowy acc to 1 bo mnożenie!, 0 będzie zerować!

}

println("factorialTR")
factorialTR(5)
factorialTR(10)
factorialTR(15)



// gcd: greatest common denominator

@tailrec
def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

println("gcd")
gcd(50, 25)
gcd(50, 20)
gcd(50, 17)


// higher order functions
def sumF(f: Int => Int, lBound: Int, hBound: Int): Long = if (lBound > hBound) 0 else f(lBound) + sumF(f, lBound + 1, hBound)
def mulF(f: Int => Int, lBound: Int, hBound: Int): Long = if (lBound > hBound) 1 else f(lBound) * mulF(f, lBound + 1, hBound)

sumF(x => x, 1, 10)
mulF(x => x, 1, 10)

// tail recursive
def sumFTR(f: Double => Double, lBound: Int, hBound: Int): Double = {

  @tailrec
  def sumFIt(acc: Double, step: Double): Double =
    if (step > hBound) acc
    else sumFIt(acc + f(step), step + 1)

  sumFIt(0, lBound)

}

sumFTR(x => x, 1, 10)


def mulFTR(f: Double => Double, lBound: Int, hBound: Int): Double = {

  @tailrec
  def mulFIt(acc: Double, step: Double): Double =
    if (step > hBound) acc
    else mulFIt(acc * f(step), step + 1)

  mulFIt(1, lBound)

}

mulFTR(x => x, 1, 10)


sumFTR(x => 1/x, 1, 10000)
mulFTR(x => 1+1/x, 1, 10000)


// Higher order functions
// ----------------------

// Funkcja addF bierze inną funkcję jako argument
def addF(f: Int => Int, a: Int, b: Int) = f(a) + f(b)
// z lambda expr
addF(x => x*x, 1, 5)

def addFC(f: (Int, Int) => Int)(a: Int, b: Int) = f(a, b)
addFC((x, y) => x*y)(1, 5)


// Funkcja zwraca funkcję
def retFun: (Int, Int) => Int = {
  def retF(xa: Int, xb: Int): Int = xa * xb

  retF
}

val returned_f = retFun
returned_f
returned_f(10, 5)




// Curring

def dzialanie(f: (Int, Int) => Int)(a: Int, b: Int): Int = f(a, b) - a * a

dzialanie((x, y) => x+y)(15, 18)
dzialanie((x, y) => x+y)(1, 18)
dzialanie((x, y) => x+y)(3, 18)

dzialanie((x, y) => x*y)(15, 18)
dzialanie((x, y) => x*y)(1, 18)
dzialanie((x, y) => x*y)(3, 18)


def sqrtF(a: Double, tol: Double = 1e-18): Double = {

  def abs(v: Double): Double = if (v < 0) -v else v
  def isGoodEnough(v: Double): Boolean = if (abs(v * v - a) / a < tol) true else false
  def improve(v: Double): Double = (v + a / v) / 2

  def sqrtIter(v: Double): Double = if (isGoodEnough(v)) v else sqrtIter(improve(v))

  sqrtIter(1.0)

}

sqrtF(4)
sqrtF(44)

// Tuple w Scala
val tup = (2, 3)
tup._1
tup._2