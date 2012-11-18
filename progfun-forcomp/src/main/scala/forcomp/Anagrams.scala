package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    val l = w.toLowerCase();
    //    l.distinct.map(c => (c, l.filter(_==c).length)).toList.sortWith(_._1<_._1)
    l.groupBy(c => c).toList.map(x => (x._1, x._2.length)).sortWith(_._1 < _._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.fold("")(_ + _))
  }

  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    //    	val os = dictionary.map(wordOccurrences(_)).distinct;
    //    	os.map(o => (o, dictionary.filter(wordOccurrences(_)==o))).toMap
    //    val m = dictionary.map(w => (wordOccurrences(w), w));
    dictionary.groupBy(wordOccurrences(_))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences.getOrElse(wordOccurrences(word), List())

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    //    val p = occurrences.inits.toList.union(occurrences.tails.toList).distinct;
    //    p.flatMap(subsets(_))
    //    val max = occurrences.maxBy(_._2)._2;
    //    List.range(0, max).toList.map(i => List.range(0, scala.math.pow(2, occurrences.length).toInt)
    ss(occurrences)
  }

  def ss(o: Occurrences): List[Occurrences] = {
    if (o.isEmpty) List(List())
    else {
      val x = o.head; val xs = o.tail;
      val s = (for (i <- 0 until x._2) yield (x._1, i + 1)).toList;
      val prepend = for (a <- s; b <- ss(xs)) yield a :: b;
      prepend ++ ss(xs)
    }
  }

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    y.foldLeft(x.toMap)((xs, a) => {
      val d = xs.apply(a._1) - a._2;
      if (d > 0) xs.updated(a._1, d) else xs.filterKeys(_ != a._1)
    }).toList.sortWith(_._1 < _._1)
  }

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    // length of the sentence
    //    val len = sentence.foldLeft(0)((s, w) => s + w.length());
    val s = sentenceOccurrences(sentence);
    // subsets of occurrences
    val cs = combinations(s);
    //    val t = cs.foldLeft((s, List():List[Word]))((os, o) => 
    //      	(subtract(os._1, o), os._2 ++ dictionaryByOccurrences.getOrElse(o, List()))); // nested tuples as arguments in lambda not possible?
    // map from word length to list of words
    //    val mw = cs.foldLeft(Map():Map[Int,List[Word]])((m, o) => {
    //      val w = dictionaryByOccurrences.getOrElse(o, List());
    //      if(w.isEmpty) m else m.updated(w.head.length(), w)
    //    });
    // find all combinations of words that have the length of the sentence

    //    val t = cs.foldLeft(s, List(): List[Sentence])((a, c) => {
    //      val xs = a._1; val r = a._2;
    //      if (xs.isEmpty) (xs, r) else {
    //        val ws = dictionaryByOccurrences.getOrElse(c, List()); // found words for combination
    //        if (ws.isEmpty) (xs, r) else { // if there are no words for the combination, we just continue
    //          // otherwise we subtract the combination from xs
    //          val xsr = subtract(xs, c);
    //          val cs = combinations(xsr); // possible combinations for the rest
    //          (xsr, r)
    //        }
    //      }
    //    });
    //    t._2
    sa(s, cs, List(List()))
  }

  def sa(os: Occurrences, cs: List[Occurrences], r: List[Sentence]): List[Sentence] = {
    if (os.isEmpty) r else { // base case
      cs match {
        case Nil => Nil // fail: no combinations left for rest
        case c :: csr => {
          val ws = dictionaryByOccurrences.getOrElse(c, List()); // list of found words for combination
          if (ws.isEmpty) sa(os, csr, r) else { // fail: no words for combination
            val osr = subtract(os, c);
            val csrr = combinations(osr); // possible combinations for the rest
            val nr = if (r.isEmpty) List(ws) else
              for (a <- ws; b <- r) yield a :: b; // new r with all words prepended
            sa(osr, csrr, nr) ++ sa(os, csr, r)
          }
        }
      }
    }
  }

};






















