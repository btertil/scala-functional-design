// Array i String to collections wzięte z Javy, w Scali nie są natywne, jedynie: Seg => List, Vector, Map
// Vector lepsze w random access, listy w head i tail tyko. Reszta taka sama, jedynie dla wektoróe nie ma :: a +: i :+ jeśłi chcemy wstawić element na początku albo na końcu

// Są immutable, czyli tworzy się nowy wektor gdy jest zmiana (podobnie było z listami)

// Map to jak dict() w Pythonie

// dodatkowo jeszcze są range,


val s = "Hello World!"

s.map(List(".>" , _))
s.flatMap(".>" + _)