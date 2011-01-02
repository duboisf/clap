package org.reliant.clap

import scala.collection
import collection.mutable.HashSet

class OptionSpec (val name: String) {
  private var short: Option[String] = None
  private var long: Option[String] = None
  private var meta: Option[String] = None

  def setShort(_short: String) = {
    short = Some(_short)
    this
  }

  def setLong(_long: String) = {
    long = Some(_long)
    this
  }

  def setMeta(_meta: String) = {
    meta = Some(_meta)
    this
  }

  def matches(target: String) = List(short, long) exists {_ exists (_ equals target)}
}

class ClapException extends Exception

class BadOptionException extends ClapException

class Clap {

  private val options = new HashSet[OptionSpec]()

  def addOption(opt: OptionSpec) {
    options += opt
  }

  type Options = List[OptionSpec]
  type Args = List[String]

  private def parseLongOpt(elem: String): (String, Option[String]) =
    elem count (_ == '=') match {
      case 0 => (elem, None)
      case 1 => {
        val name :: value :: Nil = elem split '=' toList
        (name, Some(value))
      }
      case _ => throw new BadOptionException
    }

  def parse(line: String) = {
    def parseElem(elements: List[String]): (Options, Args) = elements match {
      case head :: tail => head match {

      }
    }
    def parseElems(elements: List[String], opts: Options, args: Args): (Options, Args) = elements match {
      case Nil => (opts, args)
      case head :: tail => options find {_.matches(head)} match {
        case Some(option) => parseElems(tail, option :: opts, args)
        case _ => parseElems(tail, opts, args)
      }
    }
    parseElems(line split ' ' toList, Nil, Nil)
  }
}

object Main extends Application {
  val opt = new OptionSpec("debug") setShort "d" setLong "debug"
  println(opt matches "d")
}
