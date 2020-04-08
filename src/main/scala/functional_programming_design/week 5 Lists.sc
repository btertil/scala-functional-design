import com.sun.org.apache.xalan.internal.xsltc.compiler.util.IntType
import jdk.nashorn.internal.runtime.regexp.joni.constants.StringType

import scala.annotation.tailrec

val a = List(1, 2, 3)
val b = List(4, 5 ,6)

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

// tutaj jest odwrotnie bo najpierw będzie b (dlatego preoend)
a.:::(b)

// eksperymenty
def removeAt[T](i: Int, x: List[T]): List[T] = x.take(i) ++ x.drop(i+1)

removeAt(0, List())
removeAt(0, List(1))
removeAt(1, List(1, 2, 3))
removeAt(2, List(1, 2, 3))
removeAt(5, List(1, 2, 3))

def experyment[T](i: Int, x: List[T]): List[T] = x match {
    case List() => throw new Error("Cannot remove from empty list")
    case List(x) => if (i == 0) List() else throw new Error("Index outside of a List")
    case xs :: xn => if (i >= x.length) throw new Error("Index outside of a List") else {
        if (i >= 1) xs :: removeAt(i-1, xn) else List(xs)
    }

    //@tailrec
    //def ()
}


//removeAt(0, List())
experyment(0, List(1))
experyment(0, List(1, 2, 3))
experyment(2, List(1, 2, 3))


// quick sort with arge and tuple pattern matching

def msortInt(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
        def mergeInt(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                    if (x < y) x :: mergeInt (xs1, ys) else y :: mergeInt(ys1, xs)

        }

        val (fst, snd) = xs.splitAt(n)
        mergeInt(msortInt(fst), msortInt(snd))

    }
}

val nums = msortInt(List(1, 8, 9, 10, 21, -34, 55))
msortInt(nums)


// String version
def msortString(xs: List[String]): List[String] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
        def mergeText(xs: List[String], ys: List[String]): List[String] = (xs, ys) match {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if (x < y) x :: mergeText(xs1, ys) else y :: mergeText(ys1, xs)
        }

        val (fst, snd) = xs.splitAt(n)
        mergeText(msortString(fst), msortString(snd))

    }
}

val friuts = List("apple", "pinapple", "orange", "banana")
msortString(friuts)


// Parametrized
def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
        def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if (lt(x, y)) x :: merge(xs1, ys) else y :: merge(ys1, xs)
        }

        val (fst, snd) = xs.splitAt(n)
        merge(msort(fst)(lt), msort(snd)(lt))

    }
}

msort(nums)((x, y) => x < y)
msort(friuts)((x, y) => x < y)


// Parametrized implicit
import scala.math.Ordering

def msort_implicit[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {

        def merge_implicit(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if (ord.lt(x, y)) x :: merge_implicit(xs1, ys) else y :: merge_implicit(ys1, xs)
        }

        val (fst, snd) = xs.splitAt(n)
        merge_implicit(msort_implicit(fst), msort_implicit(snd))

    }
}

msort_implicit(nums)
msort_implicit(friuts)