package hkd

import scala.quoted.{Type, Expr, Quotes}
import scala.deriving._
import scala.compiletime.{summonFrom, erasedValue, summonInline, error}
import scala.annotation.implicitNotFound


def typeString[T: Type](using Quotes): Expr[String] = Expr(Type.show[T])

inline def showType[T]: String = ${typeString[T]}

inline def iterLabelsWrong[T]: String =  ${typeString[T]}

def iterErrMacro[T: Type](using ctx: Quotes): Expr[Nothing] =
    ctx.reflect.report.throwError(s"bad type ${Type.show[T]}")

inline def customErr[U](s: String): Nothing = ${customErrMacro[U]('s)}
def customErrMacro[U: Type](s: Expr[String])(using ctx: Quotes): Nothing = 
    ctx.reflect.report.throwError(s"${s.valueOrError} ${Type.show[U]}")



@implicitNotFound("${s} is not a label")
trait IsLabel[s]:
  def label: String
end IsLabel  

given [s <: String] (using v: ValueOf[s]) : IsLabel[s] with
  def label = v.value


inline def iterLabels[T]: List[String] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => 
      summonInline[IsLabel[t]].label :: iterLabels[ts]

inline def caseClassLabels[T](p: Mirror.ProductOf[T]): List[String] = iterLabels[p.MirroredElemLabels]
inline def matchCaseClass[T]: String = summonFrom {
  case p: Mirror.ProductOf[T] => caseClassLabels(p).mkString("(", ", ", ")")
  case _ => "non product"
}