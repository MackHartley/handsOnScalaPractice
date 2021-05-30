object Primary extends App {

  val x = Array(1,2,3,4,5)
//  println(x.mkString(" "))

  def optionthing(first: String, last: Option[String]): Unit = {
    last match {
      case Some(name) => println(s"Hey there $first $name")
      case None => println(s"What up, $first")
    }
  }

  def optionthing2(first: String, last: Option[String]): Unit = {
    println(s"how are you? $first ${last.getOrElse("<No last name>")}")
  }

//  optionthing2("Mack", None)

  val x2 = for(i <- Range(1, 10)) yield (i * i) + 1
//  println(x2.mkString(" "))

  val strings = Array("one", "two", "three")
  val nums = Array(1, 2, 3)
  val x3 = for(i <- nums; j <- strings) yield s"$j $i"
//  println(x3.mkString(" "))

  val x4 = for{
    i <- nums
    j <- strings
  }yield s"$j $i"
//  println(x4.mkString(" "))

  def myLoop(from: Int, to: Int)(callback: Int => Unit): Unit = {
    for (i <- Range(from, to)){
      callback(i)
    }
  }
//  myLoop(1, 5){
//    println(_)
//  }

  class Msg(val id: Int, val parent: Option[Int], val txt: String)

  def printMessages(messages: Array[Msg]): Unit = {
    for (msg <- messages) {
      val spaces = " " * getElevation(Some(msg), messages)
      println(s"${spaces}${msg.txt}")
    }
  }

  def getElevation(message: Option[Msg], allMessages: Array[Msg]): Int = {
    message match {
      case Some(thing) => {
        thing.parent match {
          case Some(parentId) => getElevation(allMessages.find(_.id == parentId), allMessages) + 2
          case None => 0
        }
      }
      case None => 0
    }
  }

  def coolerPrintMessages(messages: Array[Msg]): Unit = {
    def doCoolThing(parent: Option[Int], indent: String): Unit = {
      for (msg <- messages if parent == msg.parent) {
        println(s"${indent}${msg.txt}")
        doCoolThing(Some(msg.id), indent+"  ")
      }
    }
    doCoolThing(None, "")
  }

//  coolerPrintMessages(Array(
//    new Msg(0, None, "Hello"),
//    new Msg(1, Some(0), "World"),
//    new Msg(2, None, "I am Cow"),
//    new Msg(3, Some(2), "Hear me moo"),
//    new Msg(4, Some(2), "Here I stand"),
//    new Msg(5, Some(2), "I am Cow"),
//    new Msg(6, Some(5), "Here me moo, moo")
//  ))

  val elems = Array(1,2,3,4,5,6,7,8,9)
  val newThings = elems.map(_ + 1).filter(_ % 2 == 0).slice(1, 3)
  val viewThings = elems.view.map(_ + 1).filter(_ % 2 == 0).slice(1, 3).to(Array)
//  println(s"${newThings.mkString(" ")}\n${viewThings.mkString(" ")}")


  val firstMap = Map("one" -> 1, "two" -> 2, "three" -> 3)
  firstMap.contains("one")
  val keysMaybe = Array("one", "two", "four")
//  for (keyMaybe <- keysMaybe) {
//    println(firstMap.getOrElse(keyMaybe, s"Key <$keyMaybe> not found"))
//  }

  val list = List(1,2,3,4)
//  println(list.mkString(" "))
//  println(list(2))

  def isValidSudoku(grid: Array[Array[Int]]): Boolean = {
    !Range(0, 9).exists{i =>
      val row = Range(0, 9).map(grid(i)(_))
      val col = Range(0, 9).map(grid(_)(i))
      val square = Range(0, 9).map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3))
      row.distinct.length != row.length ||
        col.distinct.length != col.length ||
        square.distinct.length != square.length
    }
  }

  def isValidSudoku2(grid: Array[Array[Int]]): Boolean = {
    !Range(0, 9).exists{i =>
      val row = Range(0, 9).map(grid(i)(_)).filterNot(_ == 0)
      val col = Range(0, 9).map(grid(_)(i)).filterNot(_ == 0)
      val square = Range(0, 9).map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3)).filterNot(_ == 0)
      row.distinct.length != row.length ||
        col.distinct.length != col.length ||
        square.distinct.length != square.length
    }
  }

  val res = isValidSudoku2(Array(
    Array(3, 1, 6,   5, 7, 8,   4, 9, 2),
    Array(5, 2, 9,   1, 3, 4,   7, 6, 8),
    Array(4, 8, 7,   6, 2, 9,   5, 3, 1),

    Array(2, 6, 3,   0, 1, 0,   0, 8, 0),
    Array(9, 7, 4,   8, 6, 3,   0, 0, 5),
    Array(8, 5, 1,   0, 9, 0,   6, 0, 0),

    Array(1, 3, 0,   0, 0, 0,   2, 5, 0),
    Array(0, 0, 0,   0, 0, 0,   0, 7, 4),
    Array(0, 0, 5,   2, 0, 6,   3, 0, 0)
  ))

//  println(res)

//  def equate(mathString: String): Unit = {
//    mathString match {
//      case s"$left + $right" => equate(left) + equate(right)
//    }
//  }

  trait Writer[T]{ def writeStr(thing: T): String }
  object Writer {
    implicit object IntWriter extends Writer[Int] { def writeStr(thing: Int) = thing.toString }
    implicit object StrWriter extends Writer[String] { def writeStr(thing: String) = thing.toString }
    implicit object BoolWriter extends Writer[Boolean] { def writeStr(thing: Boolean) = thing.toString }

    implicit def WriteSeqBroken[T: Writer](): Writer[Seq[T]] = {
      new Writer[Seq[T]] {
        def writeStr(thing: Seq[T]): String = thing.map(implicitly[Writer[T]].writeStr).mkString("[",",","]")
      }
    }
    implicit def WriteSeq[T](implicit writer: Writer[T]): Writer[Seq[T]] = {
      new Writer[Seq[T]] {
        def writeStr(thing: Seq[T]): String = thing.map(writer.writeStr).mkString("[",",","]")
      }
    }
  }
  def writeToString[T](thing: T)(implicit writer: Writer[T]): String = writer.writeStr(thing)
  println(writeToString(Seq(1,2,3,4)))













  trait StrWriter[T]{ def write(t: T): String }
  object StrWriter{
    implicit object WriteInt extends StrWriter[Int]{
      def write(s: Int) = s.toString
    }
    implicit object WriteBoolean extends StrWriter[Boolean]{
      def write(s: Boolean) = s.toString
    }
    implicit object WriteDouble extends StrWriter[Double]{
      def write(s: Double) = s.toString
    }
    implicit def ParseSeq[T](implicit w: StrWriter[T]) = new StrWriter[Seq[T]]{
      def write(t: Seq[T]) = t.map(w.write).mkString("[", ",", "]")
    }

    implicit def WriteTuple[T, V](implicit w1: StrWriter[T], w2: StrWriter[V]) =
      new StrWriter[(T, V)]{
        def write(t: (T, V)) = {
          val (left, right) = t
          "[" + w1.write(left) + "," + w2.write(right) + "]"
        }
      }
  }

  def writeToString2[T](t: T)(implicit writer: StrWriter[T]): String = writer.write(t)

  def writeToConsole[T](t: T)(implicit writer: StrWriter[T]): Unit = {
    scala.Console.out.println(writer.write(t))
  }

  println(writeToString2(Seq(1,2,3,4)))
}
