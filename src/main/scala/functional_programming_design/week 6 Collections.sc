import scala.annotation.tailrec



// Array i String to collections wzięte z Javy, w Scali nie są natywne, jedynie: Seg => List, Vector, Map
// Vector lepsze w random access, listy w head i tail tyko. Reszta taka sama, jedynie dla wektoróe nie ma :: a +: i :+ jeśłi chcemy wstawić element na początku albo na końcu

// Są immutable, czyli tworzy się nowy wektor gdy jest zmiana (podobnie było z listami)

// Map to jak dict() w Pythonie

// dodatkowo jeszcze są range,


val s = "Hello World!"

// czy jakieś są z wielkiej litery a czy wszystkie są z wielkiej litery?
s.exists(_.isUpper)
s.forall(_.isUpper)

// Przykłąd map i flatmap

// Specjalnie List żeby było kilk list w sekwencji
s.map(List(".>", _))

// ale flatmap zrobi contatenacjeę tych list
s.flatMap(List(".>",  _))


val a = Vector(1, 2, 3, 4)
val b = Vector("a", "b", "c", "d")

val pairs = a.zip(b)

pairs.unzip

// Range: operator "to" inclusive, "until" exclusive !!!
1 to 10
1 until 10

// generator <- też jest inclusive ale generuje vector a nie Range
for (i <- a) println(i)
val to =  for (i <- a) yield i

1 to 5 map(_ + 2)

(1 to 5) flatMap(x => (1 to 3) map (y => (x, y)))

// scalar product z higher order functions
a.zip(a.map(_ * 5)).map(xy => xy._1 * xy._2).sum
a.map(x => x * x * 5).sum

def simpleIsPrime(n: Int): Boolean = (2 to math.sqrt(n).toInt + 1) forall (d => n % d != 0)

simpleIsPrime(27)
simpleIsPrime(7)


def isPrime(n: Int): Boolean = {

    assert(n >= 2, "Nie można sprawdzać liczby mniejszej niż 2")
    val enough = math.sqrt(n).toInt + 1

    @tailrec
    def check(step: Int = 2): Boolean = {
        if (enough < 2 || step > enough) true; else
        if (n % step != 0) check(step+1) else false
    }

    check()

}

isPrime(3)
isPrime(4)
isPrime(79)
isPrime(43)
isPrime(1277)


// przykład z generacją par i znajdowaniem par liczb, których suma jest primem

val n = 7
val pairs2 = (1 until n).flatMap(i => (1 until i).map(j => (i, j)))

pairs2.filter(pair => isPrime(pair._1 + pair._2))

// w comprehension if musi być w nawiasie za generatorem a nie jak w pythonie na końcu
for (i <- 1 to 10 if i % 2 == 0) yield i


// bardziej czytelne niż z flatmap i filter!
for (i <- 1 until n; j <- 1 until i if isPrime(i+j)) yield (i, j)

// scalar product z for expr
(for ((x, y) <- a.zip(a.map(_ * 5))) yield x * y).sum
