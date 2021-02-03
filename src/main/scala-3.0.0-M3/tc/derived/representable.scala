package tc
package derived
package representable
import hkd._


import scala.compiletime._
import scala.deriving._

inline def deriveRepresentable[U[f[_]] <: Product]: RepresentableK[U] = 
    val pack = repPack[U].toArray
    new { def tabulate[F[_]](gain: [A] => Rep[U, A] => F[A]) = summonFrom{
      case p: Mirror.ProductOf[U[F]] =>           
          p.fromProduct(new {
              def productArity = pack.length
              def canEqual(that: Any) = true
              def productElement(i: Int) = 
                type R[f[_]]
                val rep = pack(i).asInstanceOf[RepresentableK[R]]
                rep.tabulate([A] => (r: Rep[R, A]) => 
                  gain[A]([G[_]] => (ug: U[G]) => r(ug.productElement(i).asInstanceOf[R[G]])))
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
    def representable = monoCraft
    def eq = summon[U[F] =:= F[A]]

  given [V[f[_]], F[_]] (using V: RepresentableK[V]): UnapplyRep[F, V[F]] with
    type U[f[_]] = V[f]
    def representable = V
    def eq = summon[U[F] =:= V[F]]


given monoCraft[X]: Craft[[F[_]] =>> F[X]] with
  def craft[F[+_]: Applicative, G[_]](gain: [A] => ([G[_]] => G[X] => G[A]) => F[G[A]]): F[G[X]] = 
    gain([G[_]] => (x: G[X]) => x)