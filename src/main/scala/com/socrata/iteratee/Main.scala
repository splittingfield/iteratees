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
    val possibleFullWords = fullString.split(" +").toList.filter(_.nonEmpty)
    @tailrec
    def loop(words:List[String], newIter:Iteratee[String,T]):Either[WordEnumeratee[T],T] = {
      words match {
        case Nil => Left(new WordEnumeratee[T](None,newIter))
        case hd::Nil =>
          if (fullString.endsWith(" "))
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

class WordCountIteratee2(counts:Map[String,Int]) extends Iteratee[String,Map[String,Int]] {
  override def endOfInput() = counts
  override def process(word:String) = Left(new WordCountIteratee2(counts.updated(word.toLowerCase,counts.getOrElse(word.toLowerCase,0)+2)))

}


class ZipEnumeratee[T,O1,O2](it1:Iteratee[T,O1], it2:Iteratee[T,O2]) extends Iteratee[T,(O1,O2)] {
  def endOfInput = (it1.endOfInput(), it2.endOfInput())
  def process(input:T) = {
    it1.process(input) match {
      case Right(result) => it2.map((result,_)).process(input)
      case Left(newA) =>
        it2.process(input) match {
          case Right(result) => Left(newA.map((_,result)))
          case Left(newB) => Left(new ZipEnumeratee(newA,newB))
      }
    }
  }
}

object Main {
  def main(args:Array[String]) {

    val wci = new WordCountIteratee(Map[String,Int]())
    val wc2 = new WordCountIteratee2(Map[String,Int]())
    val wce = new WordEnumeratee(None,wci)
    val zip = new ZipEnumeratee(wci,wc2)
    print(">> ")
    val ln = Console.readLine()
//   val a = ln.split('!').foldLeft(zip:Iteratee[String,(Map[String,Int], Map[String,Int])]){ case (enum,word) =>
    //      enum.process(word) match {
    //        case Left(notDone) => notDone
    //        case Right(done) => enum
    //      }

    val a = ln.split('!').foldLeft(wce){ case (enum,word) =>
        enum.process(word) match {
          case Left(notDone) => notDone
          case Right(done) => enum
        }
    }


    println(a.endOfInput())


  }

}
