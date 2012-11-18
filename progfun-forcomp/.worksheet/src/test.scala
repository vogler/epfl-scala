object test {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(57); 
  println("Welcome to the Scala worksheet")
  type Occurrences = List[(Char, Int)];$skip(445); 

  def ss(o: Occurrences): List[Occurrences] = {
    if(o.isEmpty) List(List())
    else {
      var x = o.head; var xs = o.tail;
      var s = (for(i <- 0 until x._2) yield (x._1, i+1)).toList;
      var t = ss(xs);
//      var rest = if(t.length == 1 && t.head.isEmpty) List.range(0, s.length).map(List()) else t;
      var prepend = for(a <- s; b <- t) yield a::b;
      (List()::prepend)++t
    }
  };System.out.println("""ss: (o: test.Occurrences)List[test.Occurrences]""");$skip(226); val res$0 = 

  {
    var o = List(('a', 2), ('b', 2));
    var x = o.head; var xs = o.tail;
    var s = (for (i <- 0 until x._2) yield (x._1, i + 1)).toList
    var prepend = for (a <- s; b <- ss(xs)) yield a :: b;
    //s
    ss(o)
  };System.out.println("""res0: List[test.Occurrences] = """ + $show(res$0))}
}