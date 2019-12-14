
val a = List(1, 2, 3)
val b = List(4,5,6)

def initB[T](l: List[T]): List[T] = l match {
    case List() => throw new Error("Empty List")
    case List(x) => List()
    case h :: t => h :: initB(t)
}

initB(a)

a ++ b
b ++ a

a :: b

a ::: b
b ::: a
