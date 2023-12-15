package data
import cats.Monoid
import cats.syntax.monoid._
import tc._

package const:
    opaque type Const[X, +A] = X

    object Const:
        def apply[X](x: X): Const[X, Nothing] = x

        given [X: Monoid] : Applicative[[a] =>> Const[X, a]] with
            def pure[A](x: A) = Monoid.empty[X]
            extension [A, B] (xa: Const[X, A])
                override def map(f: A => B): Const[X, B] = xa
            extension [A, B, C](xa: Const[X, A])
                def map2(xb: Const[X, B])(f: (A, B) => C) = xa |+| xb

        extension [A] (x: Const[A, Any]) def value: A = x