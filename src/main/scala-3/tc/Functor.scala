package tc

import scala.language.experimental.namedTypeArguments

type Identity[+A] = A

trait Functor[F[_]]:
  extension [A, B](fa: F[A])
    def map(f: A => B): F[B]

object Functor:
  given AppTraverse[Identity] with
    override def pure[A](a: A) = a
    extension [A, B, C](a: A)
      def map2(b: B)(f: (A, B) => C) = f(a, b)

    extension [A, B, F[_]](ta: A)
      def traverse(f: A => F[B])(using F: Applicative[F]): F[B] = f(ta)

  given AppTraverse[Vector] with
    extension [A, B, F[_]](ta: Vector[A])
      def traverse(f: A => F[B])(using F: Applicative[F]): F[Vector[B]] = 
        ta.foldLeft(F.pure(Vector.empty[B]))((acc, a) => acc.map2(f(a))(_ :+ _))

    def pure[A](a: A) = Vector(a)

    extension [A, B, C](ta: Vector[A])
      def map2(tb: Vector[B])(f: (A, B) => C) = for(a <- ta; b <- tb) yield f(a, b)

  given [F[_]] (using F: cats.Applicative[F]): Applicative[F] with
    def pure[A](a: A) = F.pure(a)
    extension [A, B, C](a: F[A])
      def map2(b: F[B])(f: (A, B) => C) = F.map2(a, b)(f)

trait Pure[F[_]]:
  def pure[A](a: A): F[A] 

def pure[F[_], A](a: A)(using F: Pure[F]): F[A] = F.pure(a)

trait Apply[F[_]] extends Functor[F]:
    extension [A, B, C](fa: F[A])
      def map2(fb: F[B])(f: (A, B) => C): F[C]


trait Applicative[F[_]] extends Apply[F] with Pure[F]:
    extension [A, B](fa: F[A])
        override def map(f: A => B): F[B] = fa.map2(pure(()))((a, _) => f(a))

    extension [A, B](fa: F[A]) def zip2(fb: F[B]): F[(A, B)] = fa.map2(fb)((_, _))
    
      

trait Traverse[T[_]] extends Functor[T]:
  extension [A, B, F[_]: Applicative](ta: T[A])
    def traverse(f: A => F[B]): F[T[B]]

  extension [A, F[+_]: Applicative](ta: T[F[A]])
    def sequence: F[T[A]] = ta.traverse(fa => fa)

  extension [A, B](ta: T[A])
    override def map(f: A => B): T[B] = ta.traverse[F = Identity](f)

trait AppTraverse[F[+_]] extends Traverse[F] with Applicative[F]



    