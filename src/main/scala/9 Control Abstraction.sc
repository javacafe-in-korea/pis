import java.io.{File, PrintWriter}

//http://booksites.artima.com/programming_in_scala_2ed/examples/html/ch09.html

def withPrintWriter(file: File)(op: PrintWriter => Unit) {
  val writer = new PrintWriter(file)
  try {
    op(writer)
  } finally {
    writer.close()
  }
}

val file = new File("date.txt")

withPrintWriter(file) {
  writer =>
    writer.println(new java.util.Date)
    writer.print("")
    writer.print(true)
}


def first(x: Int) = (y: Int) => x + y
def first_full(x: Int) : (Int) => Int = { (y:Int) => x + y }


















