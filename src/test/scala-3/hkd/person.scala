package hkd
import tc._
import cats._
import _root_.data.const.Const
import scala.language.experimental.namedTypeArguments
import scala.annotation.unchecked.uncheckedVariance


type Tag = String
type Encoder[_]

case class PersonOf[F[_]](
    firstName: F[String],
    lastName: F[Option[String]],
    age: F[Int],
    tags: F[List[Tag]],
) derives Craft



def nones[Data[f[_]]](using HKD: PureK[Data]): Data[Option] = 
    HKD.pureK([A] => () => None)

given [Data[f[_]]](using HKD: Craft[Data]): Monoid[Data[Option]] with
    val empty = HKD.pureK([A] => () => None)

    def combine(x: Data[Option], y: Data[Option]): Data[Option] = 
        x.map2K(y)([A] => (xo: Option[A], yo: Option[A]) => yo.orElse(xo))

trait ShowK[F[_]]:
    given showK[A: Show]: Show[F[A]]

trait EqK[F[_]]:
    given eqK[A: Eq]: Eq[F[A]]

trait EncoderK[F[_]]:
    given encoderK[A: Encoder]: Encoder[F[A]]    

object ShowK:
    given ShowK[Identity] with 
        def showK[A: Show] = summon
    
    given ShowK[Option] with
        def showK[A: Show] = implicitly

    given [A: Show] : ShowK[[B] =>> Either[A, B]] with
        def showK[B: Show] = implicitly

def hkdToVector[G[+_], Data[f[_]]](xs: Data[G])
  (using TraversableK[Data]): Vector[G[Any]] = 
    xs.traverseK(
    	[A] => (x:  G[A]) => Const[Vector[G[Any]]](Vector(x))
    ).value

import cats.syntax.show.given

given [Data[f[_]] <: Product: Craft, F[_]]( using
    names: Data[Name], 
    shows: Data[Show],
    showK: ShowK[F]): Show[Data[F]] with
    def show(xs: Data[F]): String = 
        import showK.given
        val fields: Data[[A] =>> String] = 
            xs.map2Given(shows)([A] => (fa: F[A]) => (x: Show[A]) ?=> fa.show)
        val prefix = xs.productPrefix
        val pairs: Data[[A] =>> String] = 
            names.map2K(fields)([A] => (name: String, fld: String) => s"$name: $fld")
        hkdToVector(pairs).mkString(s"$prefix{", ",", "}")

given PersonOf[Name] = names[PersonOf]
given PersonOf[Show] = provision[PersonOf[Show]] 

@main def personMain =     
    val person = PersonOf[Identity](
        "Oleg",
        Some("Nizhnik"),
        36,
        List("scala", "love")
    )
    println(person.show)