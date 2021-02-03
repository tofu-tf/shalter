package hkd
import tc._
import scala.deriving._

type Identity[+A] = A

case class Noxarpe[F[_]](
  name: F[String],
  age: F[Int],
) derives RepresentableK


case class Jupale[F[_]](
  args: Noxarpe[F],
  ulpu: Noxarpe[F],
  jimma: F[Vector[String]],
  rec: F[() => Jupale[Identity]]
) derives RepresentableK


// given RepresentableK[Noxarpe] with 
//   def tabulate[F[_]](f: [A] => Rep[Noxarpe, A] => F[A]): Noxarpe[F] = 
//     Noxarpe(f([f[_]] => (n: Noxarpe[f]) => n.name), f([f[_]] => (n: Noxarpe[f]) => n.age) )

@main def gogogo =
  // println(showType[String])
  // println(matchCaseClass[String])
  // println(matchCaseClass[Noxarpe[Identity]])
  val nox = Noxarpe[Identity]("lol", 2)
  lazy val jup: Jupale[Identity] = Jupale(
    args = nox,
    ulpu = nox.copy(name = "kek"),
    jimma = Vector("assmu"),
    rec = () => jup,
  )

  val lsts = nox.mapK([A] => (a: A) => List(a))
  val jups = jup.mapK[Identity, [A] =>> (A, Int)]([A] => (a: A) => (a, 1))

  println(lsts)
  println(jups)



