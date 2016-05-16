//http://booksites.artima.com/programming_in_scala_2ed/examples/html/ch11.html

def f(b: Boolean) = if(b) "true" else false

1.0
1.0.hashCode
1.0.##
1
1.hashCode
1.##
1f
1f.hashCode
1f.##

()

def error = throw new RuntimeException()
def except = throw new RuntimeException()

class Sample {
  def run = println("RUN");"RUN"
}

val r :Sample = if (true) throw new RuntimeException else new Sample

r.run