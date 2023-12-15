// package tc
// import scala.deriving._
// import scala.quoted._

// inline def tabulateProduct[U[f[_]], F[_], Labels, Elems](gain: [A] => Rep[U, A] => F[A], create: Product => U[F]): U[F] = 
//   ${ tabulateMacro[U, F, Labels, Elems]('gain, 'create) }

// def tabulateMacro[U[f[_]]: Type, F[_]: Type, Labels: Type, Elems: Type](
//   gain: Expr[[A] => Rep[U, A] => F[A]],
//   create: Expr[Product => U[F]]
//   )(using qtx: Quotes): Expr[U[F]] = TabulateMacro[U, F, Labels, Elems](gain, create).result

// class Kek[F[_]](val zuzu: Int)

// class TabulateMacro[U[f[_]]: Type, F[_]: Type, Labels: Type, Elems: Type](
//   gain: Expr[[A] => Rep[U, A] => F[A]],
//   create: Expr[Product => U[F]],  
//   )(using ctx: Quotes):
//   import ctx.reflect.{_, given}
//   private val U = Type.of[U]
//   private val F = Type.of[F]
//   def result: Expr[U[F]] = 
//     val u = Type.of[U]
//     val repr = TypeRepr.of[U[F]]

//     val s = s"$labels $repr"
//     val tree = '{[F[_]] => (u: Kek[F]) => u.zuzu }.asTerm
//     val elems = Expr.ofTupleFromSeq(labels.zip(elemTypes).map{ case (label, t) =>
//        type X
//        given Type[X] = t.asType.asInstanceOf[Type[X]]
//        labeledRep[X](label)
//     })


//     u match 
//       case _ => '{
//           println(${Expr(s)})
//           println($elems)
//           $create($elems)
//         }

//   private def labeledRep[X: Type](label: String): Expr[F[X]] = 
//     def getBody[G[_]: Type](t: Expr[U[G]]): Expr[G[X]] = 
//       Select.unique(t.asTerm, label).asExpr.asExprOf[G[X]]
    
//     '{ $gain[X]([G[_]] => (u : U[G]) => ${getBody[G]('u)}) }


//   private def labels: Vector[String] = 
//     val Empty = TypeRepr.of[EmptyTuple].widen
//     Vector.unfold(TypeRepr.of[Labels].dealias){
//       case AppliedType(_, List(ConstantType(StringConstant(s)), rest)) => Some((s, rest.widen))
//       case Empty => None
//       case t => report.throwError(s"expecting string constant tuple, got $t")
//     }

//   private def elemTypes: Vector[TypeRepr] = 
//     val Empty = TypeRepr.of[EmptyTuple].widen
//     Vector.unfold(TypeRepr.of[Elems].dealias){
//       case AppliedType(_, List(AppliedType(_, List(t)), rest)) => Some((t, rest.widen))
//       case Empty => None
//       case t => report.throwError(s"expecting F[X] tuple, got $t")
//     }