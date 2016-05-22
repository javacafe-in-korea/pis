package me.placebo

sealed abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr

/**
  * Created by pLacebo on 2016. 5. 22..
  */
class Expression extends App {
  def describe(e:Expr) : String = e match {
    case Var("x") => "x"
  }
W}
