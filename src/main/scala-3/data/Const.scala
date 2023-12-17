package data

import cats.{Applicative, Monoid}
import cats.syntax.monoid._
import tc._

opaque type Const[X, +A] = X

object Const:
    def apply[X](x: X): Const[X, Nothing] = x

    given [X: Monoid] : Applicative[[a] =>> Const[X, a]] with
      def pure[A](x: A) = Monoid.empty[X]

      override def ap[A, B](ff: Const[X, A => B])(fa: Const[X, A]): Const[X, B] =
        fa

    //extension [A, B, C](xa: Const[X, A])
        //def map2(xb: Const[X, B])(f: (A, B) => C) = xa |+| xb

    extension [A, B] (x: Const[A, B]) def value: A = x