package forcomp

import forcomp.Anagrams._

object test2 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(102); 
  println("Welcome to the Scala worksheet");$skip(764); 
  def sa(os: Occurrences, cs: List[Occurrences], r: List[Sentence]): List[Sentence] = {
    if (os.isEmpty) r else { // base case
      cs match {
        case Nil => Nil // fail: no combinations left for rest
        case c::csr => {
          val ws = dictionaryByOccurrences.getOrElse(c, List()); // list of found words for combination
          println(c);
          if(ws.isEmpty) sa(os, csr, r) else { // fail: no words for combination
            val osr = subtract(os, c);
            val csrr = combinations(osr); // possible combinations for the rest
            val nr = if(r.isEmpty) List(ws) else
              for (a <- ws; b <- r) yield a :: b; // new r with all words prepended
            sa(osr, csrr, nr)
          }
        }
      }
    }
  };System.out.println("""sa: (os: forcomp.Anagrams.Occurrences, cs: List[forcomp.Anagrams.Occurrences], r: List[forcomp.Anagrams.Sentence])List[forcomp.Anagrams.Sentence]""");$skip(40); 
  val sentence = List("Linux", "rulez");System.out.println("""sentence  : List[java.lang.String] = """ + $show(sentence ));$skip(29); val res$0 = 
  sentenceAnagrams(sentence);System.out.println("""res0: List[forcomp.Anagrams.Sentence] = """ + $show(res$0))}
}