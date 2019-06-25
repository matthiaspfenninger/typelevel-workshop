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
  type ChessPiece = (String, Integer)
  //sealed trait Piece
  //sealed trait Position

  // Write a function using pattern matching that takes a square and returns whether it can move there or not

  // Model a data type that stores time spans
  type TimeSpan = (Integer, Integer)

  // Write a function that adds two TimeSpan values together

  // List all values of the type `Unit`
  def allValuesUnit: Set[Unit] = Set( () )

  // List all values of the type `Nothing`
  def allValuesNothing: Set[Nothing] = Set()

  // List all values of the type `Boolean`
  def allValuesBoolean: Set[Boolean] = Set(true, false)

  // List all values of the type `Size`
  def allValuesSize: Set[Size] = Set(Small, Medium, Large)

  // List all values of the type `(Size, Boolean)`
  def allValuesTuple: Set[(Size, Boolean)] = Set(
    (Small, true), (Medium, true), (Large, true), 
    (Small, false), (Medium, false), (Large, false))

  // List all values of the type `Either[Size, Boolean]`
  def allValuesEither: Set[Either[Size, Boolean]] = Set(
    Left(Small), Left(Medium), Left(Large),
    Right(false), Right(true)
  )

  // List all values of the type `(Size, Unit)`
  def allValuesTupleUnit: Set[(Size, Unit)] = Set((Small, ()), (Medium, ()), (Large, ()))

  // List all values of the type `Either[Boolean, Nothing]`
  def allValuesEitherNothing: Set[Either[Boolean, Nothing]] = Set(
    Left(true), Left(false)
  )


  //The Law of Iso:
  // from ∘ to ≅ id meaning ∀ a:A . (from ∘ to) a ≡ a
  // to ∘ from ≅ id meaning ∀ b:B . (to ∘ from) b ≡ b
  sealed abstract case class Iso[A,B](to: A => B, from: B => A)

  object Iso{
    def apply[A,B](to: A => B)(from: B => A):Iso[A,B] = new Iso(to, from) {}
  }

  //think about what the Size of `A` is and what the size of `(A,Unit)` is. What does this mean?
  def tupleUnit[X]:Iso[X,(X,Unit)] = Iso((x:X) => (x, ())) (tpl => tpl._1)

  //the same as above. Compare and contrast with how we saw `Either[A,B]` and `(A,B)` behave in terms of resulting size and the size of `Nothing` and `Unit`  
  def eitherNothing[X]:Iso[X,Either[X,Nothing]] = Iso(
    (x:X) => Left(x): Either[X, Nothing]) (_ match { // you have to tell scala here that it is an Either[X, Nothing], otherwise it will pick the most specific type which would be Left[X]
      case Left(x) => x
      case Right(_) => ??? // this code can never be reached, hence the ??? are actually allowed! would be "Absurd" in Haskell. you could also do a throw error here
    })

  // what you often have in good libraries is helper objects like this which which enable you to leave out the Either[...] above
  /*
  object eitherNothing {
    def left[X](x:X):Either[X, Nothing] = Left(x)
  }
  */

  def eitherNothingViaFold[X]:Iso[X,Either[X,Nothing]] = Iso(
    (x:X) => Left(x): Either[X, Nothing]) (_.fold(l => l, r => ???)) // is the same as the pattern matching above, just via fold
  

  //OPTIONAL: If you want to convince yourself further about those properties there's a couple more proofs we can write
  //These get progressively harder and need deeper insight into what's going on.

  def asocTuple[A,B,C]:Iso[((A,B),C), (A,(B,C))] = Iso((tpl: ((A, B), C)) => ((tpl._1._1), (tpl._1._2, tpl._2))) (tpl => ((tpl._1, tpl._2._1), tpl._2._2))
  
  def asocEither[A,B,C]:Iso[Either[Either[A,B],C], Either[A,Either[B,C]]] = Iso( (_:Either[Either[A,B],C]) match {
    case Left(Left(a)) => Left(a)
    case Left(Right(b)) => Right(Left(b))
    case Right(c) => Right(Right(c))
  }) (_ match {
    case Left(a) => Left(Left(a))
    case Right(Left(b)) => Left(Right(b))
    case Right(Right(c)) => Right(c)
  })

  def dist[A,B,C]:Iso[Either[(A,B), (A,C)], (A, Either[B,C])] = Iso((_: Either[(A,B), (A,C)]) match {
    case Left((a,b)) => (a, Left(b))
    case Right((a,c)) => (a, Right(c))
  }) (_ match {
    case (a, Left(b)) => Left((a, b))
    case (a, Right(c)) => Right((a, c))
  })

  sealed trait SizeTwo
  final case object One extends SizeTwo
  final case object Two extends SizeTwo

  //Arbitraty proofs can also be done
  // map None <-> Right(()) and Some(A) <-> Left(A)
  def optEither[X]:Iso[Option[X],Either[X,Unit]]= Iso((x:Option[X]) => 
    x.fold[Either[X, Unit]](Right(()))(l => Left(l))) (_.fold(l => Option(l), r => Option.empty))
  
  // could also write Some(l) instead of Option(l), and None instead of Option.empty (but type inference is easier when done like above)
  // nice thing: Option(something) gives you either back Some(something) or None if the something was Null, so use that constructor in productive code

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
