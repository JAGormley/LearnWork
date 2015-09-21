import common._
import forcomp.Anagrams.Word
import forcomp.Anagrams.dictionary
import forcomp.Anagrams.Sentence
import forcomp.Anagrams.Occurrences
object Tester2 {

  def wordOccurrencesTest(w: Word): Occurrences = {
    val temp = w.toList.groupBy((element: Char) => element)
    val temp2 = temp map {case (key, value) => (key, value.length)} toList

    temp2 sortBy(x => (x._1))
  }

  def sentenceOccurrencesTest(s: Sentence): Occurrences = {
    wordOccurrencesTest(s.mkString)
  }

  lazy val dictionaryByOccurrencesTest: Map[Occurrences, List[Word]] = {
    dictionary.groupBy((element: Word) => wordOccurrencesTest(element))
  }

  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrencesTest(wordOccurrencesTest(word))
  }

  wordAnagrams("ramp")

  sentenceOccurrencesTest(List("ddgfdgad", "esdfsdeds", "xcxmvkj"))


}

