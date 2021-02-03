package tc
import scala.compiletime._
import scala.deriving._
import scala.quoted._
import hkd._

type Rep[-U[f[_]], A] = [F[_]] => U[F] => F[A]


trait FunctorK[U[f[_]]]:
  extension [F[_], G[_]] (obj: U[F]) 
    def mapK (f: [A] => F[A] => G[A]): U[G]

trait ApplyK[U[f[_]]] extends FunctorK[U]:
  extension [F[_], G[_], H[_]] (left: U[F])
    def map2K(right: U[G])(f: [A] => (F[A], G[A]) => H[A]): U[H]

trait ApplicativeK[U[f[_]]] extends ApplyK[U]:
  def pureK[F[_]](gen: [A] => () => F[A]): U[F]

  extension [F[_], G[_]] (obj: U[F])
    override def mapK(f: [A] => F[A] => G[A]): U[G] = 
      obj.map2K[F, [A] =>> Unit, G](pureK([A] => () => ()))([A] => (x: F[A], y: Unit) => f(x))

trait RepresentableK[U[f[_]]] extends ApplicativeK[U]:
  def tabulate[F[_]](gain: [A] => Rep[U, A] => F[A]): U[F]

  override def pureK[F[_]](gen: [A] => () => F[A]): U[F] = 
    tabulate([A] => (rep: Rep[U, A]) => gen[A]())

  extension [F[_], G[_], H[_]](left: U[F])
    def map2K(right: U[G])(f: [A] => (F[A], G[A]) => H[A]): U[H] = 
      tabulate([A] => (rep: Rep[U, A]) => f[A](rep(left) , rep(right)))

object RepresentableK:
  inline def derived[U[f[_]]]: RepresentableK[U] = new  {
    def tabulate[F[_]](gain: [A] => Rep[U, A] => F[A]) = summonFrom{
      case p: Mirror.ProductOf[U[F]] => tabulateProduct[U, F, p.MirroredElemLabels, p.MirroredElemTypes](gain, p.fromProduct)
      case _ => error("can handle only case classes at the moment")
    }
  }


    