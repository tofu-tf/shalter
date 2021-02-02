package hkd
import tc._
import scala.deriving._

type Identity[+A] = A

case class Noxarpe[F[_]](
  name: F[String],
  age: F[Int],
) 
derives RepresentableK

// given RepresentableK[Noxarpe] with 
//   def tabulate[F[_]](f: [A] => Rep[Noxarpe, A] => F[A]): Noxarpe[F] = 
//     Noxarpe(f([f[_]] => (n: Noxarpe[f]) => n.name), f([f[_]] => (n: Noxarpe[f]) => n.age) )

@main def gogogo =
  // println(showType[String])
  // println(matchCaseClass[String])
  // println(matchCaseClass[Noxarpe[Identity]])
  val nox = Noxarpe[Identity]("lol", 2)
  val lsts = nox.mapK([A] => (a: A) => List(a))
  println(lsts)
  val zz = summon[Mirror.ProductOf[Noxarpe[Identity]]]
  println(zz.fromProduct(("String", 2)))

