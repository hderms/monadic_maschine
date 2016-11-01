package com.gol
import scala.collection.immutable.Map


import cats.free.Free
import cats.free.Free._
import cats.{Id, ~>}


case class ListZipper[A](val left: List[A], val right: List[A], val focus: A) { self =>
  def leftNext: ListZipper[A]= {
    val head::tail = left
    new ListZipper(focus = head, left=tail, right = right ::: List(focus))

  }

  def rightNext: ListZipper[A]= {
    val head::tail = right
    new ListZipper(focus = head, right=tail, left = left ::: List(focus))

  }

  def toList = {
    left ++ List(focus) ++ right
  }
}
object Maschine {




  sealed trait Data[A] 
  case class  Cell[A](contents: A) extends Data[A]
  case class  Register[A](contents: A) extends Data[A]
  case class  Address[A](addr: A) extends Data[A]
  case class RegisterSet3[A](op1: Register[A], op2: Register[A], op3: Register[A]) extends Data[A]
  case class RegisterSet2[A](in: Register[A], out: Register[A]) extends Data[A]
  // An algebra of primitive operations in the context of a random number generator
  sealed trait VirtualOp[A]
  object VirtualOp {
    case class Push(data: Cell[Int])                extends VirtualOp[Cell[Int]]
    case class Pop (reg: Register[Int])                extends VirtualOp[Register[Int]]
    case class Jump(address: Address[Int])                extends VirtualOp[Address[Int]]
    case class Mov(registerSet: RegisterSet2[Int])                 extends VirtualOp[RegisterSet2[Int]]
    case class  Add(registerSet: RegisterSet3[Int]) extends VirtualOp[RegisterSet3[Int]]
    case class  Sub(registerSet: RegisterSet3[Int]) extends VirtualOp[RegisterSet3[Int]]

  }

  // Free monad over the free functor of VirtualOp. The instance is not inferrable.
  type Virtual[A] = Free[VirtualOp, A]

  // Smart constructors for Virtual[A]
  def push(data: Cell[Int] ) = Free.liftF(VirtualOp.Push(data))
  def pop(reg: Register[Int] ) = Free.liftF(VirtualOp.Pop(reg))
  def jump(address: Address[Int])             = Free.liftF(VirtualOp.Jump(address))
  def add(reg: RegisterSet3[Int] ) = Free.liftF(VirtualOp.Add(reg))
  def mov(reg: RegisterSet2[Int])    = Free.liftF(VirtualOp.Mov(reg))
  def sub(reg: RegisterSet3[Int])    = Free.liftF(VirtualOp.Sub(reg))


  /*

  // Now we have enough structure to run a program
  def runVirtual[A](program: Virtual[A], seed: Long): A =
    Free.runFC[VirtualOp, RandomReader, A](program)(toState).apply(new Random(seed))

  // Syntax
  implicit class VirtualOps[A](ma: Virtual[A]) {
    def exec(seed: Long): A = runVirtual(ma, seed)
    def liftIO: IO[A] = IO(System.currentTimeMillis).map(exec)
  }
   */


  def opsPrinter: VirtualOp ~> Id =
    new (VirtualOp ~> Id) {
      def apply[A](fa: VirtualOp[A]): Id[A] = fa match {
        case VirtualOp.Push(reg) =>
          println(s"pushing $reg")
          reg

        case VirtualOp.Pop(data) =>
          println(s"pushing $data")
          data
          
      }
    }


}

object Usage extends App {
 import Maschine._

  implicit def asData(foo: Int): Cell[Int] = Cell[Int](foo)
  implicit def asRegister(foo: Int): Register[Int] = Register[Int](foo)
  // An example that returns a pair of integers, a < 100, b < a and a color
  val prog: Virtual[Data[Int]] =
    for {
      a <- push(5)
      b <- pop(5)
    } yield (b)

  println(prog)
  prog.foldMap(Maschine.opsPrinter)


}
