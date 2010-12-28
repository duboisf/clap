package org.reliant.clap

import scala.collection
import collection.mutable.HashSet

class OptionSpec {
  private var short: Option[String] = None
  private var long: Option[String] = None
  private var meta: Option[String] = None

  def setShort(_short: String): OptionSpec = {
    short = Some(_short)
    this
  }

  def setLong(_long: String): OptionSpec = {
    long = Some(_long)
    this
  }

  def setMeta(_meta: String): OptionSpec = {
    meta = Some(_meta)
    this
  }

  def matches(target: String) = List(short, long) exists {_ exists (_ equals target)}
}

class Clap {

  private val options = new HashSet[OptionSpec]()

  def addOption(opt: OptionSpec) {
    options += opt
  }

  def parse(line: String) = {
    def parseElem(elements: List[String]): List[OptionSpec] = elements match {
      case Nil => Nil
      case head :: tail => options find {_.matches(head)} match {
        case Some(option) => option :: parseElem(tail)
        case _ => parseElem(tail)
      }
    }
    parseElem(line split ' ' toList)
  }
}

object Main extends Application {
  val opt = new OptionSpec setShort "d" setLong "debug"
  println(opt matches "d")
}