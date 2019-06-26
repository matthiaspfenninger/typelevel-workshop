package workshop

import simulacrum.typeclass
import workshop.typeclasses._
import workshop.abstractions.Monoidal
import workshop.typeclasses.Monoid.ops._
import workshop.monoids.Category.ops._
import workshop.monoids.Profunctor.ops._
import workshop.monoids.Monad.ops._

import scala.util.Try
import scala.concurrent.Future
import scala.io.StdIn
import java.time.MonthDay

object monoids {

  // ADDITIVE MONOIDAL

  @typeclass trait AddMonoidal[F[_]] extends Functor[F] {
    def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]

    def zero[A]: F[A]

    def combineK[A](x: F[A], y: F[A]): F[A] =
      map(sum(x, y))(_.merge)
  }


  // CATEGORY

  @typeclass trait Category[F[_, _]] {
    def compose[A, B, C](fab: F[A, B], fbc: F[B, C]): F[A, C]

    def identity[A]: F[A, A]

    def >>>[A, B, C](fab: F[A, B], fbc: F[B, C]): F[A, C] = compose(fab, fbc)

    def <<<[A, B, C](fbc: F[B, C], fab: F[A, B]): F[A, C] = compose(fab, fbc)
  }

  implicit def categoryFunction: Category[Function1] = new Category[Function1] { 
    def compose[A, B, C](fab: A => B, fbc: B => C): A => C = fab andThen fbc

    def identity[A]: Function1[A,A] = a => a
  }


  // monoidal endo category: basically a category where you stay within ONE single type
  implicit def monoidEndoCategory[F[_, _]: Category, A]: Monoid[F[A, A]] = new Monoid[F[A, A]] {
    def combine(x: F[A,A], y: F[A,A]): F[A,A] = x.combine(y)

    def empty: F[A,A] = Monoid[F[A,A]].empty
  }

  def plusOne: Int => Int = _ + 1

  def times3: Int => Int = _ * 3

  def plusOneTimes3: Int => Int = plusOne |+| times3

  def plusOneTimes3ToString: Int => String = plusOneTimes3(_).toString()


  // Different Category instances
  case class OptionFunction[A, B](apply: A => Option[B])

  case class EffectFunction[A, B](apply: A => B)


  implicit def categoryOptionFunction: Category[OptionFunction] = new Category[OptionFunction] {
    def identity[A]: OptionFunction[A, A] = OptionFunction(a => Option(a))

    def compose[A, B, C](fab: OptionFunction[A, B], fbc: OptionFunction[B, C]): OptionFunction[A, C] =
      OptionFunction { a =>
        fab.apply(a) match {
          case Some(b) => fbc.apply(b)
          case None => None
        }
      }
  }

  implicit def categoryEffectFunction: Category[EffectFunction] = ???


  // We can define real life synchronous programs without breaking referential transparency using EffectFunction

  trait Program {
    def program: EffectFunction[List[String], Unit]

    def main(args: Array[String]): Unit =
      program.apply(args.toList)
  }

  // PROFUNCTOR

  @typeclass trait Profunctor[F[_, _]] {

    // A   -- [f] --> B   -- [fbc] -->   C   -- [g] -->   D
    def dimap[A, B, C, D](fbc: F[B, C])(f: A => B)(g: C => D): F[A, D]

    def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] = dimap(fab)(identity[A])(f)

    def lmap[A, B, C](fbc: F[B, C])(f: A => B): F[A, C] = dimap(fbc)(f)(identity)
  }

  implicit def profunctorFunction: Profunctor[Function1] = new Profunctor[Function1] {
    def dimap[A, B, C, D](fac: B => C)(f: A => B)(g: C => D): A => D =
      f >>> fac >>> g
  }

  implicit def profunctorEffectFunction: Profunctor[EffectFunction] = new Profunctor[EffectFunction] {
    def dimap[A, B, C, D](fbc: EffectFunction[B,C])(f: A => B)(g: C => D): EffectFunction[A,D] = 
      EffectFunction(f andThen fbc.apply andThen g)
  }

  implicit def profunctorOptionFunction: Profunctor[OptionFunction] = new Profunctor[OptionFunction] {

    // f andthen fbc, andthen map with g (because you want to apply g to the thing inside)
    def dimap[A, B, C, D](fbc: OptionFunction[B,C])(f: A => B)(g: C => D): OptionFunction[A,D] =     
      OptionFunction((x:A) => fbc.apply(f(x)).map(g))
  }


  // the following part was skipped
  // ------------------------------------------------

  // Now try to define an EffectFunction that prompts you to type your name,
  // then reads your name from stdin and outputs a greeting with your name.
  // To do so, you can use the `readLine` and `printLine` functions from `util`.
  def consoleProgram = ???


  // We can define functions that might fail with a value

  case class FailFunction[A, B](apply: A => Either[Throwable, B])

  implicit def categoryFailFunction: Category[FailFunction] = ???

  implicit def profunctorFailFunction: Profunctor[FailFunction] = ???


  trait FailProgram {
    def program: FailFunction[List[String], Unit]

    def main(args: Array[String]): Unit =
      program.apply(args.toList) match {
        case Left(t) => throw t
        case _ => ()
      }
  }

  // Next try to define a FailFunction that reads a file name from stdin, then reads from that file and prints out its content
  // You can try using the `data/test.txt` file.
  def fileProgram = ???


  // Tasks

  type Task[A] = FailFunction[Unit, A]

  def newCompose[A, B](ta: Task[A])(f: FailFunction[A, B]): Task[B] = ???


  type OptionTask[A] = OptionFunction[Unit, A]

  def optionCompose[A, B](ta: OptionTask[A])(f: OptionFunction[A, B]): OptionTask[B] = ???

  // ------------------------------------------------


  // MONAD

  @typeclass trait Monad[F[_]] extends Monoidal[F] {
    def flatMap[A, B](fa: /* Unit => */ F[A])(f: A => F[B]): /* Unit => */ F[B]

    def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      flatMap(fa)(a => map(fb)(b => (a, b)))

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => pure(f(a)))
  }

  implicit def monadOption: Monad[Option] = new Monad[Option] {

    // opposed to before in Functor, your flatMap (respectively the f inside it) decides where your A goes
    // (either to Some or None). in the map functions, you always know that you stay within the same "bucket".
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case None => None
    }

    def unit: Option[Unit] = Some(())
  }

  implicit def monadTask: Monad[Task] = ???

  implicit def monadEither[E]: Monad[Either[E, ?]] = new Monad[Either[E, ?]]  {

    def flatMap[A, B](fa: Either[E,A])(f: A => Either[E,B]): Either[E,B] = 
      fa.fold(e => Left(e), a => f(a))

    // another way of writing the same via match case (but fold is cleaner for Either):
    /*
    def flatMap[A, B](fa: Either[E,A])(f: A => Either[E,B]): Either[E,B] = fa match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
    */

    def unit: Either[E,Unit] = Right(())
  }


  // NOTE: you cannot compose two arbitrary monads, you can only do that with monoidal, functors, functions, etc.
  // what we do here is compose two functions of a particular shape (A -> Monad(B) and B -> Monad(C)) to a new function
  // that gives us A -> Monad(C) directly

  def composeMonadFunctions[F[_]: Monad, A, B, C](x: A => F[B], y: B => F[C]): A => F[C] = 
    a => x(a).flatMap(y)

    /*
    To understand the difference between map and flatMap, look at the different signatures:

      def map[A, B]    (fa: F[A])(f: A => B   ): F[B]
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    If we would use map in the case here, we would get a F[F[C]] as a result, so one layer too much.
    In real life use cases, you often use this principle working with Lists (if you get List(List) you 
    use flatMap instead of map intuitively), but the concept can be used with anything, not just lists.
    */


  // KLEISLI

  case class Kleisli[F[_], A, B](apply: A => F[B])

  implicit def categoryKleisli[F[_]: Monad]: Category[Kleisli[F, ?, ?]] = new Category[Kleisli[F, ?, ?]] {
    
    def compose[A, B, C](fab: Kleisli[F,A,B], fbc: Kleisli[F,B,C]): Kleisli[F,A,C] = 
      Kleisli(composeMonadFunctions(fab.apply, fbc.apply))

    def identity[A]: Kleisli[F, A, A] = Kleisli(Monad[F].pure)
  }

  implicit def profunctorKleisli[F[_]: Monad]: Profunctor[Kleisli[F, ?, ?]] = new Profunctor[Kleisli[F, ?, ?]] {

    def dimap[A, B, C, D](fbc: Kleisli[F,B,C])(f: A => B)(g: C => D): Kleisli[F,A,D] = Kleisli(a => fbc.apply(f(a)).map(g))
  }


  // IO
  // NOTE: this is a really naive implementation of an IO library. it basically just says that you have
  // pieces of code, not yet executed, only to be executed when running ioobject.unsafeRun()

  // in a real case scenario, you would use a library like Cats Effect and its' IOApp

  case class IO[A](unsafeRun: () => A) {
    def map[B](f: A => B): IO[B] = IO(() => f(this.unsafeRun()))
  }

  implicit def monadIO: Monad[IO] = new Monad[IO] {
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = IO(() => f(fa.unsafeRun()).unsafeRun())

    /*
    the following solution would be a WRONG : you have to pack your overall result inside an IO again to 
    not directly execute when using flatmap. you want to only execute the outer part when calling unsafeRun,
    not already when doing flatMap

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = f(fa.unsafeRun())
    */

    def unit: IO[Unit] = IO(() => ())
  }

  // Run both effects one after another, but only return the result of the second
  def ignoreFirstResult[A, B](fa: IO[A], fb: IO[B]): IO[B] = 
    for {
      _ <- fa
      b <- fb
    } yield b
    /*
    the following solution would technically also work for most cases, but is really not recommended  
    IO( () => { fa.unsafeRun(); fb.unsafeRun() })
    */

  // Run both effects one after another, but only return the result of the first
  def ignoreSecondResult[A, B](fa: IO[A], fb: IO[B]): IO[A] = for {
    a <- fa
    _ <- fb
  } yield a 


  // Reimplement fileprogram using `IO` instead
  // Tip: You can use for-comprehensions, you can try writing a version with and without using for-comprehensions
  def fileProgramIO = ???


  // Use IO to print out each of the names given to this function
  // You can test this using `model.userList1`
  def printAll(names: List[String]): IO[Unit] = ???

}
