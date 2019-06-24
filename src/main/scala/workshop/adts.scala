package workshop

object adts {

  // Design a data type for coffee sizes, should have small medium and large
  sealed trait Size
  case object Small extends Size
  case object Medium extends Size
  case object Large extends Size

  // Model a data type for a contact that can either be an email or a phone number
  type Contact = Unit

  // Design a data type for a chess piece and its position on the chess board
  type ChessPiece = Unit

  // Write a function using pattern matching that takes a square and returns whether it can move there or not

  // Model a data type that stores time spans
  type TimeSpan = Unit

  // Write a function that adds two TimeSpan values together

  // List all values of the type `Unit`
  def allValuesUnit: Set[Unit] = ???

  // List all values of the type `Nothing`
  def allValuesNothing: Set[Nothing] = ???

  // List all values of the type `Boolean`
  def allValuesBoolean: Set[Boolean] = ???

  // List all values of the type `Size`
  def allValuesSize: Set[Size] = ???

  // List all values of the type `(Size, Boolean)`
  def allValuesTuple: Set[(Size, Boolean)] = ???

  // List all values of the type `Either[Size, Boolean]`
  def allValuesEither: Set[Either[Size, Boolean]] = ???

  // List all values of the type `(Size, Unit)`
  def allValuesTupleUnit: Set[(Size, Unit)] = ???

  // List all values of the type `Either[Boolean, Nothing]`
  def allValuesEitherNothing: Set[Either[Boolean, Nothing]] = ???


  //The Law of Iso:
  // from ∘ to ≅ id meaning ∀ a:A . (from ∘ to) a ≡ a
  // to ∘ from ≅ id meaning ∀ b:B . (to ∘ from) b ≡ b
  sealed abstract case class Iso[A,B](to: A => B, from: B => A)

  object Iso{
    def apply[A,B](to: A => B)(from: B => A):Iso[A,B] = new Iso(to, from) {}
  }

  //think about what the Size of `A` is and what the size of `(A,Unit)` is. What does this mean?
  def tupleUnit[A]:Iso[A,(A,Unit)] = ???
  //the same as above. Compare and contrast with how we saw `Either[A,B]` and `(A,B)` behave in terms of resulting size and the size of `Nothing` and `Unit`
  def eitherNothing[A]:Iso[A,Either[A,Nothing]] = ???


  //OPTIONAL: If you want to convince yourself further about those properties there's a couple more proofs we can write
  //These get progressively harder and need deeper insight into what's going on.

  def asocTuple[A,B,C]:Iso[((A,B),C), (A,(B,C))] = ???
  def asocEither[A,B,C]:Iso[Either[Either[A,B],C], Either[A,Either[B,C]]] = ???

  def dist[A,B,C]:Iso[Either[(A,B), (A,C)], (A, Either[B,C])] = ???

  sealed trait SizeTwo
  final case object One extends SizeTwo
  final case object Two extends SizeTwo

  //Arbitraty proofs can also be done
  def optEither[A]:Iso[Option[A],Either[A,Unit]]=???

  //Think about the sizes involved
  def arbitraryProof: Iso[Either[Either[Unit, Boolean], Either[Unit, Size]], Either[SizeTwo, Either[Boolean, Size]]] = Iso(
    (a:Either[Either[Unit, Boolean], Either[Unit, Size]]) => a match {
      case Left(Left(())) => Left(One:SizeTwo)
      case Right(Left(())) => Left(Two:SizeTwo)
      case Left(Right(b)) => Right(Left(b))
      case Right(Right(s)) => Right(Right(s))
    }
  )(
    b => b match {
      case Left(One) => Left(Left(()))
      case Left(Two) => Right(Left(()))
      case Right(Left(b)) => Left(Right(b))
      case Right(Right(s)) => Right(Right(s))
    }
  )

}
