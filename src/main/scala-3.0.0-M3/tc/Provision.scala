package tc
import scala.compiletime._
import scala.deriving._

inline def provision[P](using p: Mirror.ProductOf[P]): P = 
    p.fromProduct(summonAll[p.MirroredElemTypes])

type Provided[Data[f[_]], TC[_]] = Data[TC]

type Name[A] = String

type Names[Data[f[_]]] = Data[Name]

inline def names[Data[f[_]]](using p: Mirror.ProductOf[Data[Name]]) = 
    p.fromProduct(constValueTuple[p.MirroredElemLabels])

