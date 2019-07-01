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

// curried version
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


// Z przykładu Martina Oderskyego

def sumFMO(f: Int => Int) (a: Int, b: Int): Int = if (a > b) 0 else f(a) + sumFMO(f)(a+1, b)
sumFMO(x => x*x)(1, 100) // Nie jest tail recursive!


// Oraz moja wersja Tail Recursive do funkcji Martina Oderskyego
def sumFMOTR(f: Int => Int) (a: Int, b: Int): Int = {
  @tailrec
  def innerF(acc: Int, step: Int): Int = {
    if (step > b) acc else innerF(acc + f(step), step + 1)
  }
  innerF(0, a)
}

sumFMOTR(x => x)(1, 500000)



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


// curring jeszcze raz
def infunc(a: Int, b: Int): Int = a * b
def outfunc(f: (Int, Int) => Int)(a: Int, b: Int): Int = f(a, b) - 2 * (a + b)

outfunc((x, y) => x*y)(5, 6)
outfunc((x, y) => x*y+x*y)(5, 6)

outfunc(infunc)(2, 24)


def sumf2(f: Double => Double, lBound: Double, hBound: Double): Double = {
  if (lBound > hBound) 0 else f(lBound) + sumf2(f, lBound + 1, hBound)
}

sumf2(x => x, 1, 5)
sumf2(x => x, 1, 15)

sumf2(x => 1/x, 1, 5)
sumf2(x => 1/x, 1, 15)

// Tailrec version
def sumf2TR(f: Double => Double, lBound: Double, hBound: Double): Double = {
  @tailrec
  def sumf2TRIter(acc: Double, step: Double): Double = {
    if (step > hBound) acc else sumf2TRIter(acc + f(step), step + 1)
  }
  sumf2TRIter(0, lBound)
}

sumf2TR(x => x, 1, 5)
sumf2TR(x => x, 1, 15)

sumf2TR(x => 1/x, 1, 5)
sumf2TR(x => 1/x, 1, 15)


// pattern matching

val pairs: List[(Int, String)] = List((1, "Aaa"), (2, "Eee"), (3, "Kkk"), (4, "Mmm"))

pairs.foreach {p => p._2 match {
  case "Aaa" => println(s" ${p._2} to P24, id ${p._1}")
  case "Eee" => println(s" ${p._2} to P8, id ${p._1}")
  case "Kkk" => println(s" ${p._2} to P8, id ${p._1}")
  case _ => Nil
}}



// fixed point of a function:  => f(x) = x, czyli przecięcie z przekątną na wykresie

def abs(x: Double): Double = if (x > 0) x else -x

def fixedPoint(f: Double => Double, tol: Double = 1e-12)(firstGuess: Double = 1): Double = {

  @tailrec
  def iterate(guess: Double): Double = {
    val nextval = f(guess)
    if ((abs(nextval - guess) / guess) / guess < tol) nextval else iterate(nextval)
  }

  iterate(firstGuess)

}

fixedPoint(x => 1 + x/2)(1)

// Martin Odersky sqrt as a fixedPoint - przyklad curringu z osobnymi listami atgumentów
// fixPoint przyjmuje funkcję!
def sqrtMO(x: Double): Double = fixedPoint(y => (y + x/y)/2)(1)
sqrtMO(4)


// Map
val mp = Map((1, "jeden"), (2, "dwa"))
mp

val mm = Map(1 -> "raz", 2 -> "dwa")
mm

for (m <- mm) println(m._2)