package hkd
import tc._
import scala.deriving._
import scala.language.experimental.namedTypeArguments


type Identity[+A] = A

case class Noxarpe[F[_]](
  name: F[String],
  age: F[Int],
) derives Craft


case class Jupale[F[_]](
  args: Noxarpe[F],
  ulpu: Noxarpe[F],
  jimma: F[Vector[String]],
  rec: F[() => Jupale[Identity]]
) derives Craft


// given Craft[Noxarpe] with 
//   def craft[F[+_]: Applicative, G[_]](f: [A] => Rep[Noxarpe, A] => F[G[A]]): F[Noxarpe[G]] = 
//     f([f[_]] => (n: Noxarpe[f]) => n.name).map2(f([f[_]] => (n: Noxarpe[f]) => n.age))(Noxarpe(_, _))
    

@main def gogogo =
  val nox = Noxarpe[Identity]("lol", 2)
  lazy val jup: Jupale[Identity] = Jupale(
    args = nox,
    ulpu = nox.copy(name = "kek"),
    jimma = Vector("assmu"),
    rec = () => jup,
  )

  val noxs = nox.mapK([A] => (a: A) => List(a))
  val jups = jup.mapK[Identity, [A] =>> (A, Int)]([A] => (a: A) => (a, 1))

  val noxis = Noxarpe[[A] =>> Vector[Identity[A]]](
    name = Vector("Oleg", "Katya"),
    age = Vector(36, 30)
  ).sequenceK[Vector, Identity]

  println(noxs)
  println(jups)
  noxis.foreach(println)