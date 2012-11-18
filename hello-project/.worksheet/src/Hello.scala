import java.util.NoSuchElementException

object Hello {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(68); 
  val x = 1;System.out.println("""x  : Int = """ + $show(x ));$skip(31); 
  def increase(i: Int) = i + 1;System.out.println("""increase: (i: Int)Int""");$skip(14); val res$0 = 
  increase(x);System.out.println("""res0: Int = """ + $show(res$0));$skip(88); 

  def sum(xs: List[Int]): Int =
    if (xs.isEmpty) 0
    else xs.head + sum(xs.tail);System.out.println("""sum: (xs: List[Int])Int""");$skip(21); val res$1 = 
  sum(List(1, 2, 3));System.out.println("""res1: Int = """ + $show(res$1));$skip(107); 

  def max(xs: List[Int]): Int =
    if (xs.isEmpty) throw new NoSuchElementException()
    else max_(xs);System.out.println("""max: (xs: List[Int])Int""");$skip(151); 

  def max_(xs: List[Int]): Int =
    if (xs.tail.isEmpty) xs.head
    else {
      val m = max_(xs.tail)
      if (xs.head > m) xs.head else m
    };System.out.println("""max_ : (xs: List[Int])Int""");$skip(23); val res$2 = 

  max(List(1, 3, 2));System.out.println("""res2: Int = """ + $show(res$2));$skip(28); val res$3 = 
  ")(".replace("()", "1-1");System.out.println("""res3: java.lang.String = """ + $show(res$3));$skip(69); 

  def balance(chars: List[Char]): Boolean =
    balance_(chars, 0);System.out.println("""balance: (chars: List[Char])Boolean""");$skip(230); 

  def balance_(xs: List[Char], a: Int): Boolean =
    if (xs.isEmpty) a == 0
    else if (a < 0) false else {
      val i = if (xs.head.equals("(")) 1 else if (xs.head.equals(")")) -1 else 0
      balance_(xs.tail, a + i)
    };System.out.println("""balance_ : (xs: List[Char], a: Int)Boolean""");$skip(24); val res$4 = 

  balance(")".toList);System.out.println("""res4: Boolean = """ + $show(res$4));$skip(25); val res$5 = 
  ")".toList.head == '(';System.out.println("""res5: Boolean = """ + $show(res$5));$skip(12); val res$6 = 
  '('.toInt;System.out.println("""res6: Int = """ + $show(res$6));$skip(9); val res$7 = 
  
  5%2;System.out.println("""res7: Int(1) = """ + $show(res$7));$skip(27); val res$8 = 
  
  List(2,3,1).sort(_>_);System.out.println("""res8: List[Int] = """ + $show(res$8));$skip(38); val res$9 = 
  List.range(0, 4, 1).reduceLeft(_+_);System.out.println("""res9: Int = """ + $show(res$9));$skip(31); 
  val r = (0 to 4);System.out.println("""r  : scala.collection.immutable.Range.Inclusive = """ + $show(r ));$skip(20); val res$10 =  /* until */
  r.reduceLeft(_+_);System.out.println("""res10: Int = """ + $show(res$10));$skip(13); val res$11 = 
  r.map(_*2);System.out.println("""res11: scala.collection.immutable.IndexedSeq[Int] = """ + $show(res$11));$skip(6); val res$12 = 
  5/4;System.out.println("""res12: Int(1) = """ + $show(res$12));$skip(22); 
  val l = List(1,2,3);System.out.println("""l  : List[Int] = """ + $show(l ));$skip(20); 
  val a = l.toArray;System.out.println("""a  : Array[Int] = """ + $show(a ));$skip(7); val res$13 = 
  a(1);System.out.println("""res13: Int = """ + $show(res$13));$skip(21); val res$14 = 
  l.exists(9%_ == 0);System.out.println("""res14: Boolean = """ + $show(res$14))}
}