package com.socrata.iteratee

import scala.annotation.tailrec


class WordEnumeratee[T](wordFragment:Option[String], iteratee:Iteratee[String,T]) extends Iteratee[String,T] {
  override def endOfInput() = {
    wordFragment match {
      case Some(wf) =>
          iteratee.process(wf) match {
             case Left(notDone) => notDone.endOfInput()
            case Right(done) => done
          }
      case None => iteratee.endOfInput()
    }
  }

  override def process(str:String) = {
    val fullString = wordFragment match {
      case Some(string) => string + str
      case None => str
    }

    val lastStringIsFullWord = if (fullString != Nil && fullString(fullString.length-1) == ' ') true else false
    val possibleFullWords = fullString.split(" +").toList
    @tailrec
    def loop(words:List[String], newIter:Iteratee[String,T]):Either[WordEnumeratee[T],T] = {
      words match {
        case Nil => Left(new WordEnumeratee[T](None,newIter))
        case hd::Nil =>
          if (lastStringIsFullWord)
            newIter.process(hd) match {
              case Right(done) => Right(done)
              case Left(notDone) => Left(new WordEnumeratee[T](None, notDone))
            }
          else
            Left(new WordEnumeratee[T](Some(hd),newIter))
        case hd::tl => newIter.process(hd) match {
          case Right(done) => Right(done)
          case Left(notDone) => loop(tl, notDone )
         }
      }
    }
    loop(possibleFullWords,iteratee)
  }
}

class WordCountIteratee(counts:Map[String,Int]) extends Iteratee[String,Map[String,Int]] {
  override def endOfInput() = counts
  override def process(word:String) = Left(new WordCountIteratee(counts.updated(word.toLowerCase,counts.getOrElse(word.toLowerCase,0)+1)))

}


object Main {
  def main(args:Array[String]) {

    val wci = new WordCountIteratee(Map[String,Int]())
    val wce = new WordEnumeratee(None,wci)

    @tailrec
    def getInput[T](enum:WordEnumeratee[T]):WordEnumeratee[T] = {
      print(">> ");
      val ln = Console.readLine()
      if (ln == "")
        enum
      else {
        enum.process(ln) match {
          case Left(notDone) =>  getInput(notDone)
          case Right(done) => enum
        }
      }
    }

    val a = getInput(wce)
    println(a.endOfInput())


  }

}
