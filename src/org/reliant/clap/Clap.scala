package org.reliant.clap

import scala.collection
import collection.mutable.HashSet

class ClapOption {
  private var short: Option[String] = None
  private var long: Option[String] = None
  private var meta: Option[String] = None

  def setShort(_short: String) {
    short = Some(_short)
  }

  def setLong(_long: String) {
    long = Some(_long)
  }

  def setMeta(_meta: String) {
    meta = Some(_meta)
  }

  def isMatch(tentative: String) = short == tentative || long == tentative
}

class Clap {

  private val options = new HashSet[ClapOption]()

  def addOption(opt: ClapOption) {
    options += opt
  }

  def parse(line: String) = {
    def parseElem(elements: List[String]): List[ClapOption] = elements match {
      case Nil => Nil
      case head :: tail => options find {_.isMatch(head)} match {
        case Some(option) => option + parseElem(tail)
        case _ => parseElem(tail)
      }
    }
    parseElem(line split ' ')
  }
}