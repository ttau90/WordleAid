package Models

import Filters.GuessResultFilter

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

private sealed trait HasNodeMap(val nodeMap: Map[Char, NodeType])

private sealed trait NodeType(val position: Int) {
  def addWord(word: String): NodeType
  def getWords(wordFragment: String): Seq[String]
  def filterWords(filter: GuessResultFilter): NodeType
}

case class WordTrie(override val nodeMap: Map[Char, NodeType])
  extends HasNodeMap(nodeMap) {

  def getWords: Seq[String] = {
    nodeMap.values.flatMap(_.getWords("")).filter(_.nonEmpty).toSeq
  }

  def filterWords(filter: GuessResultFilter): WordTrie = {
    val updatedMap: Map[Char, NodeType] = nodeMap.map(pair => pair._1 -> pair._2.filterWords(filter))
    val mapWithoutNullNodes: Map[Char, NodeType] = updatedMap.filter(pair => !pair._2.isInstanceOf[NullNode])

    this.copy(nodeMap = mapWithoutNullNodes)
  }
}

object WordTrie {
  def apply(wordList: Seq[String]): WordTrie = {
    val wordListNoEmptyStrings: Seq[String] = wordList.filter(_.nonEmpty)

    if (wordListNoEmptyStrings.isEmpty) {
      WordTrie(Map.empty[Char, NodeType])
    } else {
      WordTrie(applyHelper(wordListNoEmptyStrings, Map.empty[Char, NodeType]))
    }
  }

  @tailrec
  private def applyHelper(wordList: Seq[String], nodeMap: Map[Char, NodeType]): Map[Char, NodeType] = {
    if (wordList.isEmpty) {
      nodeMap
    }  else {
      val nextWord: String = wordList.head

      val updatedNode: NodeType = nodeMap.get(nextWord(0)) match {
        case Some(n) => n.addWord(nextWord)
        case None => NodeFactory(nextWord)
      }

      val updatedNodeMap: Map[Char, NodeType] = updatedNode match {
        case n: NullNode => Map.empty[Char, NodeType]
        case c: CharNode => nodeMap.updated(c.data, c)
      }

      applyHelper(wordList.drop(1), updatedNodeMap)
    }
  }
}

/*
NullNode means the char was removed or indicates end of word
*/
case class NullNode(override val position: Int) extends NodeType(position) {
  override def addWord(word: String): NodeType = NodeFactory(word, this.position)
  override def getWords(wordFragment: String): Seq[String] = Seq("")
  override def filterWords(filter: GuessResultFilter): NodeType = this
}

/*
CharNode that holds a single letter of the word
*/
case class CharNode(data: Char, override val nodeMap: Map[Char, NodeType], override val position: Int, isTerminal: Boolean)
  extends NodeType(position) with HasNodeMap(nodeMap) {

  override def addWord(word: String): NodeType = {
    val nextNode: NodeType = nodeMap.get(word(1)) match {
      case Some(n) => n.addWord(word.substring(1))
      case None => NodeFactory(word.substring(1), this.position + 1)
    }

    nextNode match {
      case n: NullNode => this
      case c: CharNode => this.copy(nodeMap = nodeMap.updated(c.data, c), isTerminal = false)
    }
  }

  override def getWords(wordFragment: String): Seq[String] = {
    val updatedWordFragment: String = wordFragment + data

    if (nodeMap.isEmpty) {
      Seq(updatedWordFragment)
    } else {
      nodeMap.values.flatMap(n => n.getWords(updatedWordFragment).filter(_.nonEmpty)).toSeq
    }
  }

  override def filterWords(filter: GuessResultFilter): NodeType = {
    val updatedFilter = filter.addLetter(this.data)

    if (this.isTerminal) {
      if (updatedFilter.isFinalWordValid) {
        this
      } else {
        NullNode(this.position)
      }
    } else {
      if (!updatedFilter.isCurrentWordValid) {
        NullNode(this.position)
      } else {
        val updatedMap: Map[Char, NodeType] = nodeMap.map(pair => pair._1 -> pair._2.filterWords(updatedFilter))
        val mapWithoutNullNodes: Map[Char, NodeType] = updatedMap.filter(pair => !pair._2.isInstanceOf[NullNode])

        if (mapWithoutNullNodes.isEmpty) {
          NullNode(this.position)
        } else {
          this.copy(nodeMap = mapWithoutNullNodes)
        }
      }
    }
  }
}

/*
* Factory that will create the correct kind of node when given a String
*/
object NodeFactory {
  def apply(word: String, nodePositionOffset: Int = 0): NodeType = {
    if (word.isEmpty) {
      NullNode(nodePositionOffset)
    } else {
      val lastPosition = nodePositionOffset + word.length

      applyHelper(word.reverse, NullNode(lastPosition))
    }
  }

  @tailrec
  private def applyHelper(word: String, childNode: NodeType): NodeType = {
    if (word.isEmpty) {
      childNode
    } else {
      val currentNodeMap: Map[Char, NodeType] = childNode match {
        case n: NullNode => Map.empty[Char, NodeType]
        case c: CharNode => Map(c.data -> c)
      }

      val currentNode: NodeType = CharNode(word.charAt(0), currentNodeMap, childNode.position - 1, childNode.isInstanceOf[NullNode])

      applyHelper(word.substring(1), currentNode)
    }
  }
}