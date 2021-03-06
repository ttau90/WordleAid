import Models.*
import Filters.*
import Utils.*

import java.util
import scala.annotation.tailrec
import scala.io.StdIn

object Main extends App {
  val words: Seq[String] = FileHelperUtil.readFileToSeq("official_words.txt").get
  val wordTrie: WordTrie = WordTrie(words)

  println("Type 'exit' to quit.")
  wordleRound(wordTrie, 1)

  @tailrec
  def wordleRound(wordTrie: WordTrie, roundNumber: Int): Unit = {
    if roundNumber == 7 then {
      println("Game Over...")
      return
    }

    val wordsAsList = wordTrie.getWords

    val scoredWords: Seq[(String, Double)] = WordProcessorUtil.scoreWords(wordTrie.getWords)
    println(s"\n====== Turn $roundNumber ======")
    println(s"Top 5 words out of ${scoredWords.length}:\n ${scoredWords.take(5)}")

    val guessedWord: String = StdIn.readLine("Enter guessed word: ")

    if guessedWord == "exit" then return

    val guessedHint: String = StdIn.readLine("Enter results using [c]orrect, [i]ncorrect, and [p]otential (e.g. \"ccipi\"): ")

    if guessedHint == "exit" then return

    val translatedHint = GuessResult.translateStringToHints(guessedHint)
    val guessResult: GuessResult = GuessResult(guessedWord, translatedHint).get
    val guessFilter: GuessResultFilter = GuessResultFilter(guessResult)

    val updatedWordTrie: WordTrie = wordTrie.filterWords(guessFilter)

    wordleRound(updatedWordTrie, roundNumber + 1)
  }
}