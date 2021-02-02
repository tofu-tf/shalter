package tc
import scala.deriving._
import scala.quoted._

inline def tabulateProduct[U[f[_]], F[_], Labels](gain: [A] => Rep[U, A] => F[A], create: Product => U[F]): U[F] = 
  ${ tabulateMacro[U, F, Labels]('gain, 'create) }

def tabulateMacro[U[f[_]]: Type, F[_]: Type, Labels: Type](
  gain: Expr[[A] => Rep[U, A] => F[A]],
  create: Expr[Product => U[F]]
  )(using qtx: Quotes): Expr[U[F]] = TabulateMacro[U, F, Labels](gain, create).result

class Kek[F[_]](val zuzu: Int)

class TabulateMacro[U[f[_]]: Type, F[_]: Type, Labels: Type](
  gain: Expr[[A] => Rep[U, A] => F[A]],
  create: Expr[Product => U[F]],  
  )(using ctx: Quotes):
  import ctx.reflect.{_, given}
  private val U = Type.of[U]
  def result: Expr[U[F]] = 
    val u = Type.of[U]
    val repr = TypeRepr.of[U[F]]

    val s = s"$labels $repr"
    val tree = '{[F[_]] => (u: Kek[F]) => u.zuzu }.asTerm
    val elems = Expr.ofTupleFromSeq(labels.map(labeledRep))


    u match 
      case _ => '{
          println(${Expr(s)})
          println(${Expr(tree.toString)})
          println($elems)
          null.asInstanceOf[U[F]]
        }

  private def labeledRep(label: String) = 
    val t: Tree = Select(Ident(TermRef(TypeRepr.of[U[F]], "u")), sym(label))
    val expr = t.asExpr
    '{ [F[_]] => (u : $U[F]) => $expr }

  private def sym(name: String) = 
    Symbol.newVal(Symbol.spliceOwner, name, TypeRepr.of[Any], Flags.EmptyFlags, Symbol.noSymbol)  

  private def labels: Vector[String] = 
    val Empty = TypeRepr.of[EmptyTuple].widen
    Vector.unfold(TypeRepr.of[Labels].dealias){
      case AppliedType(_, List(ConstantType(StringConstant(s)), rest)) => Some((s, rest.widen))
      case Empty => None
      case t => report.throwError(s"expecting string constant tuple, got $t")
    }