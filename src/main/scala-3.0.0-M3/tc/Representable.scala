package tc

import hkd._
import derived.{deriveRepresentable, deriveCraft}

type Rep[-U[f[_]], A] = [F[_]] => U[F] => F[A]


trait FunctorK[U[f[_]]]:
  extension [F[_], G[_]] (obj: U[F]) 
    def mapK (f: [A] => F[A] => G[A]): U[G]

trait ApplyK[U[f[_]]] extends FunctorK[U]:
  extension [F[_], G[_], H[_]] (left: U[F])
    def map2K(right: U[G])(f: [A] => (F[A], G[A]) => H[A]): U[H]

    def map2Given(right: U[G])(f: [A] => (F[A]) => G[A] ?=> H[A]): U[H] = 
      left.map2K(right)([A] => (fa: F[A], ga: G[A]) => {
          given G[A] = ga
          f(fa)        
      })
    

trait PureK[U[f[_]]]:
  def pureK[F[_]](gen: [A] => () => F[A]): U[F]

trait ApplicativeK[U[f[_]]] extends ApplyK[U] with PureK[U]:
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
  inline def derived[U[f[_]] <: Product]: RepresentableK[U] = deriveRepresentable

trait TraversableK[U[f[_]]] extends FunctorK[U]:
  extension[F[_], G[+_], H[_]](uf: U[F])
    def traverseK(f: [A] => F[A] => G[H[A]])(using Applicative[G]): G[U[H]]

  extension[F[+_], G[_]](uf: U[[A] =>> F[G[A]]])
    def sequenceK(using Applicative[F]): F[U[G]] = uf.traverseK([A] => (a : F[G[A]]) => a)

trait Craft[U[f[_]]] extends RepresentableK[U] with TraversableK[U]:
  def craft[F[+_]: Applicative, G[_]](gain: [A] => Rep[U, A] => F[G[A]]): F[U[G]]

  def tabulate[F[_]](gain: [A] => Rep[U, A] => F[A]): U[F] = craft[Identity, F](gain)

  extension[F[_], G[+_], H[_]](uf: U[F])
    def traverseK(f: [A] => F[A] => G[H[A]])(using Applicative[G]): G[U[H]] = 
      craft[G, H]([A] => (frep: Rep[U, A]) => f(frep(uf)))

object Craft:
  inline def derived[U[f[_]] <: Product]: Craft[U] = deriveCraft    
