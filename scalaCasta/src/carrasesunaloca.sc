/*

//Para sumar uno al primer valor en la lista
List(1, 2, 3, 4) match {
  case h :: Nil => (h +1) :: Nil
  case h :: t => (h +1) :: t
  case _ => 0
}


//Para sumar 1 a toda la lista
def add1(l: List[Int]): List[Int] ={
  l match {
    case h :: Nil => (h +1) :: Nil
    case h :: t => (h +1) :: add1(t)
    case Nil => Nil
  }
}


add1(List(1,2,3,4))

//Que pasa si es una lista muy grande, se llena el Stack
//Entonces se debe mejorar


def add2(l: List[Int]): List[Int] ={
  val du = l
  l match {
    case h :: Nil => (h +2) :: Nil
    case h :: t => (h +2) :: add2(t)
    case Nil => Nil
  }
}



	import scala.annotation.tailrec

def add(l: List[Int]): List[Int] ={
  @tailrec
  def loopAdd(elm: List[Int], acc: List[Int]):List[Int] = {
    elm match {
      case h :: Nil => acc :+ (h+1)
      case h :: t => loopAdd(t, acc :+ (h+1))
      case Nil => acc
    }
  }
  loopAdd(l,Nil)
}

add(List(1,2,3,4,5,6,7,8))


//Ahora realicemos el promedio de una lista
def promedio(l: List[Int]): Double = {
  @tailrec
  def loopProm(elm: List[Int], acum: Double, size: Double):
  Double ={
    elm match {
      case h :: Nil =>  (h+acum)/size
      case h :: t => loopProm(t, (h+acum), size +1)
      case h:: Nil => acum
    }
  }
  loopProm(l, 0D, 1D)
}


promedio(List(2,8,11,3))


//val listica = List(1,2,3,4,5)
//println(listica.sum/listica.size)


//Miremos un tema diferente
//+A significa A y sus hijos

case class List[+A](c: A)

sealed trait Persona
final case class Juridica() extends Persona
final case  class Natural() extends Persona

//val ps: Const[Persona] = Const(Natural())


// O.o

/*

case class Mensaje(texto: String, sha: Int)
object Mensaje{
  def apply(texto: String, sha: Int)={
//    if(validarSha(sha)) Mensaje(texto, sha)
 //   else ???
  }
  private def validarSha(sha: Int): Boolean = ???
}
*/
//Para manejar errores

//Option(null)
//subtipos Some y None


//None me indica que el valor esta aucente
//Some --> me indica que hay algo

//Option(1)

*/


import com.sun.javafx.geom.transform.Identity

import scala.util.Try
import scala.util.{Failure, Success}

Try[Int](1)
Success(1)
Failure(new Exception)


Try[Int](throw new Exception("Error"))
Success(1)
Failure(new Exception)


//Otra forma de manejar errores
//Either[-A, +B]
Right(1)
Left(2) //EL valor que debe salir con el error, VALOR MALO

val ll: Either[String, Int] = Left("Error")

//Este debe sacar error Left
for {
  r <- Right(1)
  r1 <- Right(2)
  l <- ll
} yield r+r1+l

//El siguiente for esta bueno
for {
  r <- Right(1)
  r1 <- Right(2)
  l <- Right(3)
} yield r+r1+l


////---- SmartConstructor

trait MensajeError {val error: String}

case class Mensaje(texto: String, sha: Int)
object Mensaje{
  def apply(texto: String, sha: Int):
  Either[MensajeError, Mensaje]={
        if(validarSha(sha)) Right( new Mensaje(texto, sha))
       else Left(new MensajeError{val error = "Bad sha"})
  }
  private def validarSha(sha: Int): Boolean = true
  //En lo anterior en vez de true debe ir la condicion
  //Para que se construya el mensaje
}

//def cifrar(m: Mensaje): Mensaje = m
//Mer Mensaje("Hola",1).fold(Identity, cifrar

/*

# Preguntas (10% c/u)

1. ¿Qué acciones ***no*** debería realizar una función pura?
    Las funciones puras no deberian realizar lo siguiente:
     - No deben tomar variables globales
     - No deben llamar funciones que no sean puras
     - No deben tener datos mutables
     - Siempre deben retornar los mismos datos para la misma entrada


2. ¿Qué se gana al crear funciones puras?
    Las funciones puras no estan acopladas, por lo tanto es más sencillo realizar pruebas.
    El codigo de estas es más entendible, y debido a que no necesitan datos externos se
    pueden paralelizar de manera sencilla.
    El negocio de un proyecto deberia ser con funciones puras, debido a que con estas,
    se puede garantizar la integridad del codigo.

3. ¿Por qué diseñar datos inmutables?
    Es mejor diseñar con datos inmutables debido a que los datos mutables pueden cambiar en
    cualquier momento de estado, por lo tanto al usar datos imutables garantizamos integridad
    en los datos.

4. ¿Por qué son importantes los tipos?
    Los tipos son importantes para definir claramente con que estamos trabajando, esto nos
    ayuda a filtrar la informacion que le puede llegar a un método, y así, encontrar más facil
    errores en tiempo de complilación.

# Tests Passed(4% c/u) 

# Estilo y buenas prácticas (-1% por cada falla) 
Ejemplos de fallas:
- No reusar funciones donde aplique. Incluye funciones de orden superior.
- No usar funciones parciales para ganar expresividad donde aplique.
- No usar nombres dicientes para variables o funciones (no aplica para funciones anónimas con *underscore*).
- Las funciones con s

*/

/*



package exam

import scala.annotation.tailrec

sealed trait Collection[+A]
case object Empty extends Collection[Nothing]
final case class Cons[+A](head: A, tail: Collection[A]) extends Collection[A]

object Collection {
  def apply[A](as: A*): Collection[A] = as.toList match {
    case cons if cons.isEmpty => Empty
    case h :: t => Cons(h, apply(t: _*))
  }

  def foldRight[A, B](col: Collection[A], z: B)(f: (A, B) => B): B = col match {
    case Empty => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def length[A](col: Collection[A]): Int = foldRight(col, Integer.MIN_VALUE) { ??? }
}

object CollectionInt {
  def sumFold(col: Collection[Int]): Int = Collection.foldRight(col, ???)(???)

  def sumRec(l: Collection[Int]): Int = {
    @tailrec
    def sumAccumulator(elm: Collection[Int], acum: Int): Int = {
      elm match {
        case Cons(h, Empty) => h + acum
        case Cons(h, t) => sumAccumulator(t, (h + acum))
        case Empty => acum
      }
    }
    sumAccumulator(l, 0)
  }

  def productFold(col: Collection[Int]) = ???

  //def productRec(col: Collection[Int]) = ???

  def productRec(l: Collection[Int]): Int = {
    @tailrec
    def proAccumulator(elm: Collection[Int], acum: Int): Int = {
      elm match {
        case Cons(h, Empty) => h * acum
        case Cons(h, t) => proAccumulator(t, (h * acum))
        case Empty => acum
      }
    }
    proAccumulator(l, 1)
  }

  def maxFold(col: Collection[Int]) = Collection.foldRight(col, Integer.MAX_VALUE)(_ max _)

  def maxRec(l: Collection[Int]): Int = {
    @tailrec
    def max(elm: Collection[Int], maximo: Int):
    Int ={
      elm match {
        case Cons(h, Empty) => if (h >= maximo) h else maximo
        case Cons(h, t) => max(t, if (h >= maximo) h else maximo)
        case Empty => maximo
      }
    }
    max(l, 0)
  }
}


*/
