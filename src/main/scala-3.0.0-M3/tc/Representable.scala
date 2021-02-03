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
  inline def derived[U[f[_]] <: Product]: RepresentableK[U] = new  {
    def tabulate[F[_]](gain: [A] => Rep[U, A] => F[A]) = summonFrom{
      case p: Mirror.ProductOf[U[F]] => 
          val pack = repPack[U]
          p.fromProduct(new {
              def productArity = pack.size
              def canEqual(that: Any) = true
              def productElement(i: Int) = 
                type R[f[_]]
                val rep = pack(i).asInstanceOf[RepresentableK[R]]
                rep.tabulate([A] => (r: Rep[R, A]) => 
                  gain[A]([G[_]] => (ug: U[G]) => ug.productElement(i).asInstanceOf[G[A]]))
          })
      case _ => error("can handle only case classes at the moment")
    }
  }


inline def repPack[U[_[_]]]: Vector[RepresentableK[?]] = 
  type F[_]
  summonFrom{
    case p: Mirror.ProductOf[U[F]] => repIter[F, Widen[p.MirroredElemTypes]]
  }

inline def repIter[F[_], P]: Vector[RepresentableK[?]] = 
  inline erasedValue[P] match 
    case _ : (t *: ts) => elemRep[F, t] +: repIter[F, ts]
    case _ : EmptyTuple   => Vector()
    case t => customErr[P]("expecting a tuple got ")

inline def elemRep[F[_], T]: RepresentableK[?] = summonFrom{
  case ur : UnapplyRep[F, T] => ur.representable
}

trait UnapplyRep[F[_], T]{
  type U[f[_]]
  def representable: RepresentableK[U]
  def eq: T =:= U[F]
}

object UnapplyRep:
    given [F[_], A] : UnapplyRep[F, F[A]] with
      type U[f[_]] = f[A]
      def representable = monoRepresentable
      def eq = summon[U[F] =:= F[A]]

    given [V[f[_]], F[_]] (using V: RepresentableK[V]): UnapplyRep[F, V[F]] with
      type U[f[_]] = V[f]
      def representable = V
      def eq = summon[U[F] =:= V[F]]


given monoRepresentable[X]: RepresentableK[[F[_]] =>> F[X]] with
  def tabulate[F[_]](gain: [A] => ([G[_]] => G[X] => G[A]) => F[A]) = gain([G[_]] => (gx: G[X]) => gx)

