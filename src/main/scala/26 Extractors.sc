import java.util.Optional

// http://booksites.artima.com/programming_in_scala_2ed/examples/html/ch26.html

object EMail {
  // The injection method (optional)
  def apply(user: String, domain: String) = {
    println("Email.apply")
    user + "@" + domain
  }

  // The extraction method (mandatory)
  def unapply(str: String): Option[(String, String)] = {
    println("Email.unapply")
    val parts = str split "@"
    if (parts.length == 2) Some(parts(0), parts(1)) else None
  }
}

object Twice {
  def apply(s: String): String = {
    println("Twice.apply")
    s + s
  }
  def unapply(s: String): Option[String] = {
    println("Twice.unapply")
    val length = s.length / 2
    val half = s.substring(0, length)
    if (half == s.substring(length)) Some(half) else None
  }
}

object UpperCase {
  def unapply(s: String): Boolean = {
    println("UpperCase.unapply")
    s.toUpperCase == s
  }
}

def userTwiceUpper(s: String) = s match {
  case EMail(Twice(x @ UpperCase()), domain) =>
    "match: "+ x +" in domain "+ domain
  case _ =>
    "no match"
}



userTwiceUpper("DIDI@hotmail.com")

userTwiceUpper("DIDO@hotmail.com")

userTwiceUpper("didi@hotmail.com")


object Domain {

  // The injection method (optional)
  def apply(parts: String*): String =
    parts.reverse.mkString(".")

  // The extraction method (mandatory)
  def unapplySeq(whole: String): Option[Seq[String]] =
    Some(whole.split("\\.").reverse)
}

def isTomInDotCom(s: String): Boolean = s match {
  case EMail("tom", Domain("com", _*)) => true
  case _ => false
}

isTomInDotCom("tom@sun.com")
isTomInDotCom("peter@sun.com")
isTomInDotCom("tom@acm.org")

object ExpandedEMail {
  def unapplySeq(email: String)
  : Option[(String, Array[String])] = {
    val parts = email split "@"
    if (parts.length == 2)
      Some(parts(0), parts(1).split("\\.").reverse)
    else
      None
  }
}

val Decimal = """(-)?(\d+)(\.\d*)?""".r
val Decimal(sign, integerpart, decimalpart) = "-1.23"








val input = "for -1.0 to 99 by 3"
for (s <- Decimal findAllIn input) println(s)

Decimal findFirstIn input

Decimal findPrefixOf input

val input2 = "1.0 for -1.0 to 99 by 3"
Decimal findPrefixOf input2
