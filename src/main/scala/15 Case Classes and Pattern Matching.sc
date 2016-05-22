// http://booksites.artima.com/programming_in_scala_2ed/examples/html/ch15.html

sealed abstract class Expr

case class Var(name: String) extends Expr

case class Number(num: Double) extends Expr

case class UnOp(operator: String, arg: Expr) extends Expr

case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

def simplifyTop(expr: Expr): Expr = expr match {
  case UnOp("-", UnOp("-", e)) => e // Double negation
  case BinOp("+", e, Number(0)) => e // Adding zero
  case BinOp("*", e, Number(1)) => e // Multiplying by one
  case _ => expr
}

simplifyTop(UnOp("-", UnOp("-", Var("x"))))
simplifyTop(UnOp("-", Var("x")))

def wildcard(expr: Expr) = expr match {
  case BinOp(_, _, _) => println(expr + " is a binary operation")
  case _ => println("It's something else")
}

wildcard(UnOp("-", Var("x")))
wildcard(BinOp("-", Var("x"), Number(0)))

def describe(x: Any) = x match {
  case 5 => "five"
  case true => "truth"
  case "hello" => "hi!"
  case Nil => "the empty list"
  case _ => "something else"
}

describe(1)
describe(false)
describe("hell")
describe(Array())

def tupleDemo(expr: Any) =
  expr match {
    case (a, b, c) => println("matched " + a + b + c)
    case _ =>
  }

tupleDemo(("a ", 3, "-tuple"))

def generalSize(x: Any) = x match {
  case s: String => s.length
  case m: Map[_, _] => m.size
  case _ => -1
}

generalSize("abc")

generalSize(Map(1 -> 'a', 2 -> 'b'))

generalSize(math.Pi)

def isIntIntMap(x: Any) = x match {
  case m: Map[Int, Int] => true
  case _ => false
}

isIntIntMap(Map(1 -> 1))
isIntIntMap(Map("abc" -> "abc"))

def isStringArray(x: Any) = x match {
  case a: Array[String] => "yes"
  case _ => "no"
}

isStringArray(Array("abc"))

isStringArray(Array(1, 2, 3))

def simplifyAdd(e: Expr) = e match {
  case BinOp("+", x, y) if x == y =>
    BinOp("*", x, Number(2))
  case _ => e
}

// 15.8 More Example(345p ~)
object Element {

  private class ArrayElement(
                              val contents: Array[String]
                            ) extends Element

  private class LineElement(s: String) extends Element {
    val contents = Array(s)

    override def width = s.length

    override def height = 1
  }

  private class UniformElement(
                                ch: Char,
                                override val width: Int,
                                override val height: Int
                              ) extends Element {
    private val line = ch.toString * width

    def contents = Array.fill(height)(line)
  }

  def elem(contents: Array[String]): Element =
    new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)

  def elem(line: String): Element =
    new LineElement(line)
}

import Element.elem

abstract class Element {
  def contents: Array[String]

  def width: Int = contents(0).length

  def height: Int = contents.length

  def above(that: Element): Element = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    elem(this1.contents ++ that1.contents)
  }

  def beside(that: Element): Element = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height
    elem(
      for ((line1, line2) <- this1.contents zip that1.contents)
        yield line1 + line2)
  }

  def widen(w: Int): Element =
    if (w <= width) this
    else {
      val left = elem(' ', (w - width) / 2, height)
      var right = elem(' ', w - width - left.width, height)
      left beside this beside right
    }

  def heighten(h: Int): Element =
    if (h <= height) this
    else {
      val top = elem(' ', width, (h - height) / 2)
      var bot = elem(' ', width, h - height - top.height)
      top above this above bot
    }

  override def toString = contents mkString "\n"
}


class ExprFormatter {
  // Contains operators in groups of increasing precedence
  private val opGroups =
    Array(
      Set("|", "||"),
      Set("&", "&&"),
      Set("^"),
      Set("==", "!="),
      Set("<", "<=", ">", ">="),
      Set("+", "-"),
      Set("*", "%")
    )

  // A mapping from operators to their precedence
  private val precedence = {
    val assocs =
      for {
        i <- opGroups.indices // 0 until opGroups.length
        op <- opGroups(i)
      } yield op -> i
    assocs.toMap
  }

  private val unaryPrecedence = opGroups.length
  private val fractionPrecedence = -1

  private def format(e: Expr, enclPrec: Int): Element =
    e match {
      case Var(name) =>
        elem(name)
      case Number(num) =>
        def stripDot(s: String) =
          if (s endsWith ".0") s.substring(0, s.length - 2)
          else s
        elem(stripDot(num.toString))
      case UnOp(op, arg) =>
        elem(op) beside format(arg, unaryPrecedence)
      case BinOp("/", left, right) =>
        val top = format(left, fractionPrecedence)
        val bot = format(right, fractionPrecedence)
        val line = elem('-', top.width max bot.width, 1)
        val frac = top above line above bot
        if (enclPrec != fractionPrecedence) frac
        else elem(" ") beside frac beside elem(" ")
      case BinOp(op, left, right) =>
        val opPrec = precedence(op)
        val l = format(left, opPrec)
        val r = format(right, opPrec + 1)
        val oper = l beside elem(" " + op + " ") beside r
        if (enclPrec <= opPrec) oper
        else elem("(") beside oper beside elem(")")
    }

  def format(e: Expr): Element = format(e, 0)
}


val f = new ExprFormatter

val e1 = BinOp(
  "*",
  BinOp("/", Number(1), Number(2)),
  BinOp("+", Var("x"), Number(1)))

val e2 = BinOp(
  "+", BinOp("/", Var("x"), Number(2)),
  BinOp("/", Number(1.5), Var("x")))

val e3 = BinOp("/", e1, e2)

def show(e: Expr) = println(f.format(e) + "\n\n")

for (e <- Array(e1, e2, e3)) show(e)


println("-" * 50)

// match with not case class
object Num {
  def unapply(num: Num): Option[Int] = Some(num.num)

  def apply(num: Int) = new Num(num)
}

sealed class Num(val num: Int)

def matchCase(c: Num) = c match {
  case Num(0) => "0"
}

// println("not case class " + matchCase(Num(1))) // match error

sealed abstract class N
case class Num2(num: Int) extends N
case class Num3(num: Int) extends N

def matchCase2(n: Num2) = n match {
  case Num2(1) => "N2_1"
}
