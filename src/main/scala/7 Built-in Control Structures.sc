// http://booksites.artima.com/programming_in_scala_2ed/examples/html/ch07.html

var a = 10
var b = 20

while (a != 0) {
  val temp = a
  a = b % a
  b = temp
}


val n = 10
if (n % 2 == 0)
  n / 2
else
  throw new RuntimeException("n must be even")

val forLineLengths =
  for (
    file <- List("array")
    if file.endsWith(".scala");
    trimmed = file.trim;
    if trimmed.matches(".*for.*")
  ) yield trimmed.length

for {
  file <- List("array")
  if file.endsWith(".scala")
} yield file

val firstArg = "chips"


"salt" match {
  case "salt" => println("pepper")
  case "chips" => println("salsa")
  case "eggs" => println("bacon")
  case _ => println("huh?")
}

println(
  firstArg match {
    case "salt" => "pepper"
    case "chips" => "salsa"
    case "eggs" => "bacon"
    case _ => "huh?"
  }
)

def makeRowSeq(row: Int) =
  for (col <- 1 to 10) yield {
    val prod = (row * col).toString
    val padding = " " * (4 - prod.length)
    padding + prod
  }

def makeRow(row: Int) = makeRowSeq(row).mkString

def multiTable() = {
  val tableSeq =
    for (row <- 1 to 10)
      yield makeRow(row)

  tableSeq.mkString("\n")
}

multiTable()
