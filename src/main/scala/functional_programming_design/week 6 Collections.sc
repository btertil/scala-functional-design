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

