package workshop

import workshop.adts._
import workshop.model.rawText
import simulacrum.typeclass
import scala.concurrent.Future
import workshop.typeclasses.Show.ops._
import workshop.typeclasses.Eq.ops._
import workshop.typeclasses.Monoid.ops._
import workshop.typeclasses.Functor.ops._
import scala.concurrent.ExecutionContext.Implicits.global

object typeclasses {

  // SHOW

  // NOTE: show is meant to make things human readable, NOT to serialize stuff. only to be used for human debugging etc.

  @typeclass trait Show[A] {
    def show(a: A): String
  }

  implicit def showInt: Show[Int] = new Show[Int] {
    def show(a: Int): String = a.toString
  }

  implicit def showString: Show[String] = new Show[String] {
    def show(a: String): String = a
  }

  implicit def showChessPiece: Show[ChessPiece] = new Show[ChessPiece] {
    def show(a: ChessPiece): String = a._1.toString + " is on field " + a._2.toString
  }

  // if you have a trait with only one function definition (which we have with Show above), you can also just
  // define your implementation lambda style with Scala 2.12 (SAM)
  implicit def showOption[A: Show]: Show[Option[A]] = option => option.fold("None")(x => Show[A].show(x))


  // a Show function for tuples where each element has it's own Show
  implicit def showTpl[A: Show, B:Show]: Show[(A,B)] = tpl => "(" + tpl._1 + ", " + tpl._2 + ")"

  // a Show function for Lists where each element has it's own Show
  implicit def showLst[A: Show]: Show[List[A]] = lst => lst.map(Show[A].show).mkString("[", ",", "]")


  // EQ

  @typeclass trait Eq[A] {
    def eqv(x: A, y: A): Boolean
    def ===(x: A)(y: A): Boolean = eqv(x, y)
  }

  implicit def eqInt: Eq[Int] = new Eq[Int] {
    def eqv(x: Int, y: Int): Boolean = x == y
  }

  implicit def eqString: Eq[String] = new Eq[String] {
    def eqv(x: String, y: String): Boolean = x.equals(y)
  }

  implicit def eqOption[A: Eq]: Eq[Option[A]] = (x, y) => (x, y) match {
    case (Some(a), Some(b)) => a === b
    case (None, None) => true
    case (None, _) => false
    case (_, None) => false
  }

  implicit def eqEither[A: Eq, B: Eq]: Eq[Either[A, B]] = (x, y) => (x, y) match {
    case (Left(a1), Left(a2)) => a1 === a2
    case (Right(b1), Right(b2)) => b1 === b2
    case _ => false 
  }
  // be careful with default pattern matches! in a productive setup where you have an evolving data model
  // don't use it as you will have an exhaustive match ANYTIME and you might to forget to cover new cases



  // MONOID

  @typeclass trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A

    def |+|(x: A)(y: A): A = combine(x, y)
  }

  implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  implicit def stringMonoid: Monoid[String] = new Monoid[String] {
    def empty: String = ""

    def combine(x: String, y:String): String = x + y
  }

  implicit def timespanMonoid: Monoid[TimeSpan] = new Monoid[TimeSpan] {
    def empty: TimeSpan = new TimeSpan(0, 0)

    def combine(x: TimeSpan, y: TimeSpan): TimeSpan = new TimeSpan(x._1 + y._1, x._2 + y._2)
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def empty: List[A] = List.empty

    def combine(x: List[A], y: List[A]): List[A] = x ++ y
  }

  // The intMonoid further up uses `addition` as its method of combination, but that is not the only monoid for `Int`!
  // We can also define one for multiplication, though if we just define it for `Int` again the compiler won't know which to pick
  // What we can do instead is define a small wrapper that allows us to multiply
  case class Mult(value: Int)

  implicit def multMonoid: Monoid[Mult] = new Monoid[Mult] {
    def empty: Mult = Mult(1)

    def combine(x: Mult, y: Mult): Mult = Mult(x.value * y.value)
  }

  // foldRight is stack safe! use it wherever possible
  // beautiful simple way
  def combineAll[A: Monoid](list: List[A]): A = list.foldRight(Monoid[A].empty)(Monoid[A].combine)

  // manually handrolled way with pattern matching
  def combineAllHandrolled[A: Monoid](list: List[A]): A = list match {
    case Nil => Monoid[A].empty
    case (head :: Nil) => head
    case (head :: tl) => Monoid[A].combine(head, combineAllHandrolled(tl))
  }

  // again the same, but via our own foldMap below
  def combineAllViaFold[A: Monoid](list: List[A]): A = foldMap(list)(identity)
  
  // apply a map A=>B on every element of type A, and then fold the B elements
  def foldMap[A, B: Monoid](list: List[A])(f: A => B): B = list.foldRight(Monoid[B].empty)((x, y) => Monoid[B].combine(f(x), y))

  // tupleMonoid
  implicit def tupleMonoid[A: Monoid, B: Monoid]: Monoid[(A, B)] = new Monoid[(A, B)] {
    def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)

    def combine(x: (A, B), y: (A, B)): (A, B) = (Monoid[A].combine(x._1, y._1), Monoid[B].combine(x._2, y._2))
  }

  // tuple3Monoid
  implicit def tuple3Monoid[A: Monoid, B: Monoid, C: Monoid]: Monoid[(A, B, C)] = new Monoid[(A, B, C)] {
    def empty: (A, B, C) = (
      Monoid[A].empty, 
      Monoid[B].empty, 
      Monoid[C].empty
      )

    def combine(x: (A, B, C), y: (A, B, C)): (A, B, C) = (
      Monoid[A].combine(x._1, y._1), 
      Monoid[B].combine(x._2, y._2), 
      Monoid[C].combine(x._3, y._3)
      )
  }

  // mapMonoid
  implicit def mapMonoid[A, B: Monoid]: Monoid[Map[A, B]] = new Monoid[Map[A, B]] {
    def empty: Map[A,B] = Map.empty

    def combine(x: Map[A, B], y: Map[A, B]): Map[A, B] = y.foldRight(x)(
      (tpl, map) => map + (
        tpl._1 -> map
          .get(tpl._1)
          .fold(tpl._2)(_ |+| tpl._2)
        )
    )
  }

  implicit def futureMonoid[A: Monoid]: Monoid[Future[A]] = new Monoid[Future[A]] {
    def empty: Future[A] = Future.successful(Monoid[A].empty)

    // this combine makes the execution of x and y sequential, because 'for' is desugared to a flatMap and flatMap only allows sequential operations 
    def combine(x: Future[A], y: Future[A]): Future[A] = for {
      a <- x
      b <- y
    } yield (a |+| b)

    // this combine uses zip which allows parallel computing of the two zipped futures, therefore better than the flatMap above
    /*
    def combine(x: Future[A], y: Future[A]): Future[A] = x.zip(y).map(tpl => tpl._1 |+| tpl._2)
    */
  }


  // MONOID WORD COUNT
  //Use foldMap with a Monoid to count the number of words, the number of characters and the number of occurences of each word
  //Tip: the Map and Tuple3 Monoid can help

  val words: List[String] = rawText.split(" ").toList

  // 3tuple containing (# words, # characters, occurence counts)
  val statistics1: (Int, Int, Map[String, Int]) = foldMap(words)(word => (1, word.length, Map(word -> 1)))
  
  //Now that you have the word count let's extend it with the ability to get the longest word of the text.
  //Tip: Define a Maximum Monoid to do so
  final case class MaxLenStrings(len:Int, strings: Set[String])

  implicit def maxMonoid: Monoid[MaxLenStrings] = new Monoid[MaxLenStrings] {
    def empty: MaxLenStrings = MaxLenStrings(0, Set.empty)

    def combine(x: MaxLenStrings, y: MaxLenStrings): MaxLenStrings = 
      if (x.len > y.len) x
      else if (y.len > x.len) y
      else MaxLenStrings(x.len, x.strings union y.strings)
  }

  // 3 tuple containing (# words, # characters, words with max. length)
  val statistics2: (Int, Int, MaxLenStrings) = foldMap(words)(word => (1, word.length(), MaxLenStrings(word.length, Set(word))))


  // FUNCTOR

  @typeclass trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    //you can implement fmap directly here in the trait. It is just very handy to build an intuition about "Functors move functions into an effect"
    def fmap[A, B](f: A => B):F[A] => F[B] = fa => map(fa)(f)
  }

  implicit def optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(value) => Some(f(value))
      case None => None
    }
  }

  implicit def listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
      case head :: tl => f(head) :: map(tl)(f)
      case Nil => Nil
    }
  }

  // an example of what the above actually enables
  val listExample = listFunctor.map(List(1,2,3,4,5))(_ + 5)

  // the useful thing is that functors compose. you can do things like the following (we would need to also implement compose for that)
  /*
  val optionListFunctor = Functor[List].compose[Option]
  optionListFunctor.map(List(Option(2), Option(10), None))(_ + 10)
  */

  // go with the cats library if you want to use it in a real life use case
  /*
  import cats.Functor
  import cats.implicits._
  val listOption = List(Some(1), None, Some(2))
  Functor[List].compose[Option].map(listOption)(_ + 1)
  */


  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  implicit def treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }



  // CARDINALITY

  @typeclass trait Cardinality[A] {
    def cardinality: BigInt
  }

  implicit def cardinalityUnit: Cardinality[Unit] = new Cardinality[Unit] {
    def cardinality: BigInt = 1
  }

  implicit def cardinalityBoolean: Cardinality[Boolean] = ???

  implicit def cardinalityByte: Cardinality[Byte] = ???

  implicit def cardinalityShort: Cardinality[Short] = ???

  implicit def cardinalityInt: Cardinality[Int] = ???

  implicit def cardinalityTuple[A: Cardinality, B: Cardinality]: Cardinality[(A, B)] = ???

  implicit def cardinalityEither[A: Cardinality, B: Cardinality]: Cardinality[Either[A, B]] = ???

  implicit def cardinalitySize: Cardinality[Size] = ???

  implicit def cardinalityNothing: Cardinality[Nothing]= ??? // 0 (because you cannot instantiate a 0)

  implicit def cardinalityFunction[A: Cardinality, B: Cardinality]: Cardinality[A => B] = ??? // pow(B.cardinality, A.cardinality)



}
