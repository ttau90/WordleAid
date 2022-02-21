package Models

import Filters.GuessResultFilter

import org.scalatest.funsuite.AnyFunSuite
import scala.util.{Failure, Success, Try}

class WordTrieTest extends AnyFunSuite {
  test("CharNode creates trie of nodes") {
    val validationNode = CharNode('h',Map('e' -> CharNode('e',Map('l' -> CharNode('l',Map('l' -> CharNode('l',Map('o' -> CharNode('o',Map(), 4, true)), 3, false)), 2, false)), 1, false)), 0, false)
    val node: NodeType = NodeFactory("hello")

    assert(node == validationNode)
  }

  test("CharNode.addWord updates trie with new node(s)") {
    val validationNode = CharNode('a',Map('c' -> CharNode('c',Map(), 1, true)), 0, false)
    val trie: NodeType = NodeFactory("a")
    val updatedTrie: NodeType = trie.addWord("ac")

    assert(updatedTrie == validationNode)
  }

  test("CharNode.addWord works multiple times") {
    val validationNode = CharNode('a',Map('c' -> CharNode('c',Map(), 1, true), 'x' -> CharNode('x', Map(), 1, true)), 0, false)
    val trie: NodeType = NodeFactory("a")

    val updatedTrie: NodeType = trie.addWord("ac").addWord("ax")

    assert(updatedTrie == validationNode)
  }

  test("CharNode.getWords returns words as Seq") {
    val validationSeq: Seq[String] = Seq("ac", "ax").sorted
    val trie: NodeType = NodeFactory("a")

    val words: Seq[String] = trie.addWord("ac").addWord("ax").getWords("").sorted

    assert(words == validationSeq)
  }

  test("WordTrie is empty map if seq is empty") {
    val trie = WordTrie(Seq())

    assert(trie.nodeMap.isEmpty)
  }

  test("WordTrie is empty map if seq is empty string") {
    val trie = WordTrie(Seq(""))

    assert(trie.nodeMap.isEmpty)
  }

  test("WordTrie can add a word") {
    val validationTrie = WordTrie(Map('h' -> CharNode('h',Map('e' -> CharNode('e',Map('l' -> CharNode('l',Map('l' -> CharNode('l',Map('o' -> CharNode('o',Map(), 4, true)), 3, false)), 2, false)), 1, false)), 0, false)))

    val trie: WordTrie = WordTrie(Seq("hello"))

    assert(trie == validationTrie)
  }

  test("WordTrie can add multiple words") {
    val validationTrie = WordTrie(Map('c' -> CharNode('c',Map('a' -> CharNode('a',Map('t' -> CharNode('t',Map(), 2, true), 'b' -> CharNode('b',Map(), 2, true)), 1, false)), 0, false), 'd' -> CharNode('d',Map('o' -> CharNode('o',Map('g' -> CharNode('g',Map(), 2, true)), 1, false)), 0, false)))

    val trie: WordTrie = WordTrie(Seq("cat", "dog", "cab"))

    assert(trie == validationTrie)
  }

  test("WordTrie can print words as seq") {
    val validationSeq: Seq[String] = Seq("cat", "dog", "cab").sorted

    val words: Seq[String] = WordTrie(Seq("cat", "dog", "cab")).getWords.sorted

    assert(words == validationSeq)
  }

  test("WordTrie can filter out words with incorrect letter") {
    // Assume correct word is 'cat'
    // Assume guessed word is 'cap'

    val wordTrie: WordTrie = WordTrie(Seq("cat", "dog", "cab", "cap"))
    val hints: Vector[HintType] = Vector(HintType.Correct, HintType.Correct, HintType.Incorrect)
    val guessResult: GuessResult = GuessResult("cap", hints).get
    val filter: GuessResultFilter = GuessResultFilter(guessResult)

    val expectedWords: Seq[String] = Seq("cat", "cab").sorted

    val words = wordTrie.filterWords(filter).getWords.sorted

    assert(words == expectedWords)
  }

  test("WordTrie can filter out words with incorrect letters") {
    // Assume correct word is 'apple'
    // Assume guessed word is 'pears'
    // Words with 'r' or 's' will be removed

    val wordTrie: WordTrie = WordTrie(Seq("apple", "pears", "angry", "speak", "eaplp"))
    val hints: Vector[HintType] = Vector(HintType.Potential, HintType.Potential, HintType.Potential, HintType.Incorrect, HintType.Incorrect)
    val guessResult: GuessResult = GuessResult("pears", hints).get
    val filter: GuessResultFilter = GuessResultFilter(guessResult)

    val expectedWords: Seq[String] = Seq("apple", "eaplp").sorted

    val words = wordTrie.filterWords(filter).getWords.sorted

    assert(words == expectedWords)
  }

  test("WordTrie can filter out words with too many potential letter") {
    // Assume correct word is 'apple'
    // Assume guessed word is 'peeep'
    // Words with 1 or 3+ 'p' are removed
    
    // Look into why loppp is invalid
    val wordTrie: WordTrie = WordTrie(Seq("apple", "palle", "loppp"))
    val hints: Vector[HintType] = Vector(HintType.Potential, HintType.Potential, HintType.Incorrect, HintType.Incorrect, HintType.Potential)
    val guessResult: GuessResult = GuessResult("peeep", hints).get
    val filter: GuessResultFilter = GuessResultFilter(guessResult)

    val expectedWords: Seq[String] = Seq("apple").sorted

    val words = wordTrie.filterWords(filter).getWords.sorted

    assert(words == expectedWords)
  }

  test("WordTrie can filter out words with too many potential letter, even with a correct spot") {
    // Assume correct word is 'apple'
    // Assume guessed word is 'apeep'
    // Words with 0 or 2+ 'p's not in the 2nd position are removed

    val wordTrie: WordTrie = WordTrie(Seq("apple", "peaps", "apelle", "ppeep"))
    val hints: Vector[HintType] = Vector(HintType.Correct, HintType.Correct, HintType.Potential, HintType.Incorrect, HintType.Potential)
    val guessResult: GuessResult = GuessResult("apeep", hints).get
    val filter: GuessResultFilter = GuessResultFilter(guessResult)

    val expectedWords: Seq[String] = Seq("apple").sorted

    val temp = wordTrie.filterWords(filter)
    val words = wordTrie.filterWords(filter).getWords.sorted

    assert(words == expectedWords)
  }
}