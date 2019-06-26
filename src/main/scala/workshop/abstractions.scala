package workshop

import workshop.typeclasses._
import workshop.model._
import simulacrum.typeclass
import scala.concurrent.Future
import abstractions.Monoidal.ops._
import abstractions.Traverse.ops._
import scala.concurrent.ExecutionContext.Implicits.global
import adts.Iso

object abstractions {

  // Multiplicative Monoidal Functors

  @typeclass trait Monoidal[F[_]] extends Functor[F] { self => 
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

    def unit: F[Unit]

    // technically pure is NOT a member of Monoidal but it's just way to handy to have. 
    // Pure is a member of the formulation through applicative but as you can see we can express it easily enough
    def pure[A](a: A): F[A] = map(unit)(_ => a)

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      map(product(fa, fb)) { case (a, b) => f(a, b) }

    // NOTE: monoidals compose (not like monoids), which is also why you can use flatMap on monoidals and not on monoids
    // example: Future(Option(Int)) + Future(Option(String)) => Future(Option(Int, String))
    def compose[G[_]:Monoidal] : Monoidal[λ[X => F[G[X]]]] = new Monoidal[λ[X => F[G[X]]]]{ // λ can also be written as Lambda
      def unit: F[G[Unit]] = self.pure(Monoidal[G].unit)
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = self.map(fga)(ga => ga.map(f))
      def product[A, B](fga: F[G[A]], fgb: F[G[B]]): F[G[(A, B)]] = self.map2(fga, fgb)((ga, gb) => Monoidal[G].product(ga, gb))
    }
  }

  // NOTE: self is needed if you want to access the functions of this level in an inner level
  // if you would use Monoidal[F] instead, you are using the definition inside the definition itself, could lead to endless recursion

  // explanation that these structures are usually called APPLICATIVE and proof that it's the same:

  //An alternative (and in practice more common formulation) of Multiplicative Monoidal Functors is called APPLICATIVE but they're the same as shown below
  //The reason that Applicative is more often chosen as the cannonical representation are not known to me but I suspect it's the case because it's what Haskell does and it's shorter.
  //However most of the things that we use in Scala with Applicative really rely more on its nature as a product monoidal rather than its capability of `ap`
  @typeclass trait Applicative[F[_]] extends Functor[F] {
    override def map[A,B](fa:F[A])(f:A=>B):F[B]= ap(pure(f))(fa)
    def pure[A](a:A):F[A]
    def ap[A,B](fab:F[A=>B])(fa:F[A]):F[B]
  }

  def applicativeIsSameAsMonoidal[F[_]]:Iso[Monoidal[F],Applicative[F]] = Iso(
    (m:Monoidal[F]) => new Applicative[F]{
      override def pure[A](a:A):F[A] = m.pure(a)
      override def ap[A,B](fab:F[A=>B])(fa:F[A]):F[B] = map(m.product(fab, fa))(tpl => tpl._1(tpl._2))
    }
  )(
    (a:Applicative[F]) => new Monoidal[F]{
      override def map[A, B](fa: F[A])(f: A => B): F[B] = a.map(fa)(f)
      override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = a.ap(map(fa)( a => ((b:B) => (a,b)) ))(fb)
      override def unit: F[Unit] = a.pure(())
    }
  )

  implicit def optionMonoidal: Monoidal[Option] = new Monoidal[Option] {
    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(a), Some(b)) => Some((a,b))
      case (Some(a), None) => None // you do not have ANY information about what B is so you cannot infer any standard value, hence you have to map these cases to None
      case (None, Some(b)) => None
      case (None, None) => None
    }

    def unit: Option[Unit] = Some(())
    
    //def map[A, B](fa: Option[A])(f: A => B): Option[B] = optionFunctor.map(fa)(f)
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(value) => Some(f(value))
      case None => None
    }
  }

  implicit def futureMonoidal: Monoidal[Future] = new Monoidal[Future] {
    def product[A, B](fa: Future[A], fb: Future[B]): Future[(A, B)] = fa.zip(fb)

    def unit: Future[Unit] = Future.successful(())

    def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

  implicit def listMonoidal: Monoidal[List] = new Monoidal[List] {
    def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa match {
      case Nil => Nil
      case a_head :: a_tl => fb.map(b => (a_head, b)) ::: product(a_tl, fb)
    }

    def unit: List[Unit] = List(())

    //def map[A, B](fa: List[A])(f: A => B): List[B] = listFunctor.map(fa)(f)
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
      case head :: tl => f(head) :: map(tl)(f)
      case Nil => Nil
    }
  }

  // test cases to see what the listMonoidal.product does
  val testListProduct = listMonoidal.product(List(1,2,3), List(6,7,8)) // should contain 3x3=9 entries
  val testListProduct2 = listMonoidal.product(List(1,2,3,4,5), List(true, false)) // should contain 5x2=10 entries


  def bundle[F[_]: Monoidal, A](x: F[A], y: F[A]): F[List[A]] = Monoidal[F].map2(x, y) ((a, b) => List(a, b))

  def appendM[F[_]: Monoidal, A](x: F[A], y: F[List[A]]): F[List[A]] = Monoidal[F].map2(x, y) (_ :: _) // last bracket stands for: ((a:A, as:List[A]) => a :: as)

  def sequence[F[_]: Monoidal, A](list: List[F[A]]): F[List[A]] = list.foldRight(Monoidal[F].pure(List.empty[A]))(
    (fa, flista) => appendM(fa, flista))

  def traverse[F[_]: Monoidal, A, B](list: List[A])(f: A => F[B]): F[List[B]] = list.foldRight(Monoidal[F].pure(List.empty[B]))(
    (a, flistb) => appendM(f(a), flistb))

  def sequence2[F[_]: Monoidal, A](list: List[F[A]]): F[List[A]] = traverse(list)(identity)

  def ap[F[_]: Monoidal, A, B](ff: F[A => B], fa: F[A]): F[B] = Monoidal[F].map2(ff, fa)((f, a) => f(a))

  // testcases
  val testBundle = bundle(Option(3), Option(4)) // == Some(List(3,4))
  val testAppend = appendM(Option(4), Option(List(2, 3))) // == Some(List(4,2,3))
  val testSequence = sequence(List(Option(3), Option(4), Option(5))) // == Some(List(3,4,5))
  val testTraverse = traverse(List(1,2,3))(a => Option(a > 2)) // == Some(List(false, false, true))
  val testSequence2 = sequence(List(Option(3), Option(4), Option(5))) // == Some(List(3,4,5))
  val testAp = ap(Option((x:Int) => x>3), Option(2)) // == Some(false)

  //Given two Option[Int] multiply the int values if they exist or leave them unchanged if one of them doesn't
  def combineOptions(x: Option[Int], y: Option[Int]): Option[Int] = ???


  // FOLDABLE

  @typeclass trait Foldable[F[_]] {
    //implementations of foldMap should only go through F ONCE
    def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

    def combineAll[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)
  }

  implicit def optionFoldable: Foldable[Option] = new Foldable[Option] {
    def foldMap[A, B: Monoid](fa: Option[A])(f: A => B): B = fa match {
      case Some(a) => f(a)
      case None => Monoid[B].empty
    }
  }

  implicit def listFoldable: Foldable[List] = new Foldable[List] {
    def foldMap[A, B: Monoid](fa: List[A])(f: A => B): B = fa match {
      case a :: lista => Monoid[B].combine(f(a), foldMap(lista)(f))
      case Nil => Monoid[B].empty
    }
  }

  implicit def setFoldable: Foldable[Set] = new Foldable[Set] {
    def foldMap[A, B: Monoid](fa: Set[A])(f: A => B): B = fa.foldRight(Monoid[B].empty)(
      (a, bs) => Monoid[B].combine(f(a), bs))
  }

  // Turn this foldable into a List
  def fromFoldable[F[_]: Foldable, A](fa: F[A]): List[A] = Foldable[F].foldMap(fa)(a => List(a))

  // Find the first element that matches the predicate
  // Hint: YOu might need to defne a new type with a new monoid
  def find[F[_]: Foldable, A](fa: F[A], f: A => Boolean): Option[A] = 
    Foldable[F].foldMap(fa)(
      a => if (f(a)) Option(a) else None) (

        new Monoid[Option[A]] {
          def empty: Option[A] = None
          def combine(x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
        }
    )

  // testcases
  val testFromFoldable1 = fromFoldable(Option(4))
  val testFromFoldable2 = fromFoldable(List(1,2,3,4))
  val testFromFoldable3 = fromFoldable(Set(1,2,3,4))  
  val testFind = find(List(1,7,3,4,9), (_:Int) > 2) // == Some(7)


  // TRAVERSABLE
  @typeclass trait Traverse[F[_]]  {
    def traverse[G[_]: Monoidal, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

    def sequence[G[_]: Monoidal, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)
  }

  implicit def listTraversable: Traverse[List] = new Traverse[List] {
    def traverse[G[_]: Monoidal, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = fa 
      .foldRight(Monoidal[G].pure(List.empty[B])) (
        (a, glistb) =>  Monoidal[G].map2(f(a), glistb) ((b:B, bs:List[B]) => b :: bs) // corresponds to the appendM from above
      )
  }

  implicit def optionTraversable: Traverse[Option] = new Traverse[Option] {
    def traverse[G[_]: Monoidal, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = fa  
      .fold(Monoidal[G].pure(Option.empty[B])) (
        a => Monoidal[G].map(f(a)) (Option(_)) // could also write a => f(a).map(Option(_))
      )
  }

  implicit def eitherTraversable[E]: Traverse[Either[E, ?]] = new Traverse[Either[E, ?]] {
    def traverse[G[_]: Monoidal, A, B](fa: Either[E,A])(f: A => G[B]): G[Either[E,B]] = fa
      .fold(e => Monoidal[G].pure(Left(e)), a => Monoidal[G].map(f(a)) (Right(_))) // could also write a => f(a).map(Right(_))
  }



  // VALIDATED

  sealed trait Validated[+E, +A]
  case class Valid[+A](a: A) extends Validated[Nothing, A]
  case class Invalid[+E](e: E) extends Validated[E, Nothing]

  type ValidatedList[+E, +A] = Validated[List[E], A]

  def toEither[E, A](v: Validated[E, A]): Either[E, A] = v match {
    case Valid(a) => Right(a)
    case Invalid(e) => Left(e)
  }

  def toValidated[E, A](e: Either[E, A]): Validated[E, A] = e match {
    case Right(a) => Valid(a)
    case Left(e) => Invalid(e)
  }

  implicit def validatedMonoidal[E: Monoid]: Monoidal[Validated[E, ?]] = new Monoidal[Validated[E, ?]] {
    
    def unit: Validated[E,Unit] = Valid(())

    def map[A, B](fa: Validated[E,A])(f: A => B): Validated[E,B] = fa match {
      case Valid(a) => Valid(f(a))
      case Invalid(e) => Invalid(e)
    }

    def product[A, B](fa: Validated[E,A], fb: Validated[E,B]): Validated[E,(A, B)] = (fa, fb) match {
      case (Valid(a), Valid(b)) => Valid((a,b))
      case (Valid(a), Invalid(e)) => Invalid(e)
      case (Invalid(e), Valid(b)) => Invalid(e)
      case (Invalid(e1), Invalid(e2)) => Invalid(Monoid[E].combine(e1, e2)) 
    }

  }

  implicit def validatedTraversable[E]: Traverse[Validated[E, ?]] = new Traverse[Validated[E, ?]] {
    def traverse[G[_]: Monoidal, A, B](fa: Validated[E,A])(f: A => G[B]): G[Validated[E,B]] = fa match {
      case Valid(a) => f(a).map(Valid(_))
      case Invalid(e) => Monoidal[G].pure(Invalid(e))
    }
  }


  // VALIDATION EXERCISES

  // Use `Validated` and everything you've learned so far to solve this one
  // In the `model` object you can find two lists of unvalidated strings representing users
  // Your job is to check all of them whether they are valid or not.
  // To do so, you should use the `User.validate` function.
  // Once your done, you can check the difference to Either

  def allUsers: List[String] = model.userList1 ++ model.userList2 // userList1: all valid - userList2: all invalid
  val validatedUsers: ValidatedList[String, List[User]] = allUsers.traverse(model.User.validate) 

  val validatedUsersRisky: ValidatedList[String, List[User]] = allUsers.map(model.User.validate).sequence 
  // NOTE 1: functionally the same as above, but in some cases this can be much more expensive than the one above
  // because you instantiate *all* of the entries. use traverse instead

  // NOTE 2: if you would use 'map', then you get a list of Validated, but you want a Validated of a list
  // hence use 'traverse' instead of 'map'


  // Next we want to write a function that takes a String representing a user
  // and return the UserReport for that user using the `User.fetchReport` function
  def reportForUser(u: String): Future[ValidatedList[String, UserReport]] = User.validate(u).traverse(User.fetchReport)

  // HARD: Now get all reports for all the users
  val allReports: Future[ValidatedList[String, List[UserReport]]] = allUsers.traverse(reportForUser).map(_.sequence)
  
  // NOTE: this has the inner functions flipped - you wan't a validated list of a userreport list
  // and NOT a list of single validated userreports. hence you need to flip the inner part with map sequence (or traverse(identity))
  val allReportsWrong: Future[List[ValidatedList[String, UserReport]]] = allUsers.traverse(reportForUser)


  // NESTED MONOIDAL

  case class Nested[F[_], G[_], A](value: F[G[A]])

  implicit def nestedMonoidal[F[_]: Monoidal, G[_]: Monoidal]: Monoidal[Nested[F, G, ?]] = new Monoidal[Nested[F, G, ?]] {

    val fgcomposit = Monoidal[F].compose[G]

    def product[A, B](fa: Nested[F,G,A], fb: Nested[F,G,B]): Nested[F,G,(A, B)] = Nested(fgcomposit.product(fa.value, fb.value))

    def map[A, B](fa: Nested[F,G,A])(f: A => B): Nested[F,G,B] = Nested(fgcomposit.map(fa.value)(f))

    def unit: Nested[F,G,Unit] = Nested(fgcomposit.unit)
  }


  // Try implementing `allReports` using `Nested`, it should be much easier this way
  def allReportsUsingNested: Future[ValidatedList[String, List[UserReport]]] = allUsers.traverse( u => Nested(reportForUser(u))).value
  def allReportsUsingNestedWrong = allUsers.map( u => Nested(reportForUser(u)))

  // EXPLANATIION: goal is to get:            List      - Future    - Validated

  // FIRST WAY via traverse & map(sequence):
  // if you use map(reportForUser):           List      - Future    - Validated       --> not what you want
  // if you use traverse(reportForUser):      Future    - List      - Validated       (flipping the first pair)
  // if you use map(_.sequence) on top:       Future    - Validated - List            (flipping the second pair)

  // SECOND WAY via traverse & Nested:
  // if you use map(Nested(reportForUser)):     List    - (Future    - Validated)
  // so the traverse flips it like that:       (Future  - Validated) - List         (flipping tuple to the front)
  // and unpacking it via value gets you:       Future  - Validated  - List         (calling .value)


  // a covariant functor provides a     map that gets you from A to B knowing the A->B projection (produces A)
  // a contravariant functor provides a map that gets you from A to B knowing the B->A projection (consumes A)
  @typeclass trait ContravariantFunctor[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  case class Predicate[A](run: A => Boolean)

  case class StringEncoder[A](run: A => String)

  implicit def predicateContravariant: ContravariantFunctor[Predicate] = new ContravariantFunctor[Predicate] {
    def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] = Predicate[B](f andThen fa.run)
  }

  implicit def stringEncoderContravariant: ContravariantFunctor[StringEncoder] = new ContravariantFunctor[StringEncoder] {
    def contramap[A, B](fa: StringEncoder[A])(f: B => A): StringEncoder[B] = StringEncoder[B](f andThen fa.run)
  }


  // THESE TWO INSTANCES ARE NOT POSSIBLE TO WRITE but looking at them can help form a better intuition about this. 
  // So if you're still a bit shaky try to implement them and see if you understand why it doesn't work
  implicit def predFunctor:Functor[Predicate] = new Functor[Predicate] {

    // you want: B -> bool
    // you have: A -> bool / A -> B
    // you need: A -> bool / B -> A, then you could build B -> A -> bool
    def map[A, B](fa: Predicate[A])(f: A => B): Predicate[B] = ???  // not possible!
  }

  implicit def optContravar:ContravariantFunctor[Option] = new ContravariantFunctor[Option] {

    // you want: Option(B)
    // you have: Option(A) / B -> A
    // you need: Option(A) / A -> B, then you could build {case Some(A) -> Some(B); case None -> None}
    def contramap[A, B](fa: Option[A])(f: B => A): Option[B] = ??? // not possible!
  }
}

// NOTE: contravariance and covariance for functions:
// ARGUMENTS of functions are CONTRAvariant     / example: f(a) -> x  &  b -> a  ==>  f(b) -> x
// RESULTS   of functions are COvariant         / example: f(a) -> x  &  x -> y  ==>  f(a) -> y