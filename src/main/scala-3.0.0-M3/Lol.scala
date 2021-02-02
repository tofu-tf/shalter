object Lol:
  val message = "3.0"


type Foo = [x] =>> Either[x, String]
type K1 = [_] =>> Any 
type KF[A <: AnyKind, B <: AnyKind] = [_ <: A] =>> B  

def foo[k1 <: AnyKind, k2 <: AnyKind, t[x <: k1] <: KF[k1, k2][x]]: Unit = ()

