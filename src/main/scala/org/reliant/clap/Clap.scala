package org.reliant.clap

abstract class OptionSpec {
  private var argument: Option[String] = None
  val argRequired = false
  def setArg(arg: String) =
    if (argRequired) argument = Some(arg) else throw new NoArgumentSupportException
  def getArg() = if (argRequired) argument else throw new NoArgumentSupportException
  def parse(elems: List[String]): (Boolean, List[String]) = (false, Nil)
}

abstract class PosixOption(val name: String) extends OptionSpec

trait Short extends OptionSpec {
  val short: String

  abstract override def parse(args: List[String]) = {
    val (success, rest) = _parse(args)
    if (success)
      (success, rest)
    else
      super.parse(args)
  }

  private def matches(arg: String) =
    (arg startsWith "-") && (short equals (arg drop 1))

  private def _parse(args: List[String]) =
    if (argRequired)
      (parseWithArg(args take 2), args drop 2)
    else
      (this matches (args head), args drop 1)

  private def parseWithArg(args: List[String]) = args match {
    case option :: value :: Nil =>
      if (this matches option) {
        this setArg value
        true
      } else false
    case _ => false
  }
}

trait Long extends OptionSpec {
  val long: String
}

trait Argument extends OptionSpec {
  override val argRequired = true
}

class ClapException extends Exception

class BadOptionException extends ClapException

class BadOptionFormatException extends ClapException

class NoOptionValueException extends ClapException

class NoArgumentSupportException extends ClapException

class NoOptionMatchException extends ClapException

class Clap {

  private def isLong(opt: String) = opt startsWith "--"

  private def isShort(opt: String) = !isLong(opt) && (opt startsWith "-")

  private def hasValue(opt: String) =
    isLong(opt) && ((opt count (_ == '=')) == 1) && !(opt endsWith "=")

  private def getValue(opt: String) =
    if (hasValue(opt)) {
      val _ :: value :: Nil = opt split '=' toList;
      Some(value)
    }
    else
      None

  private def parseLongOpt(elem: String): (String, Option[String]) =
    elem count (_ == '=') match {
      case 0 => (elem, None)
      case 1 => (elem, getValue(elem))
      case _ => throw new BadOptionException
    }

}

object Main extends Application {

  val test = new PosixOption("port") with Short with Long {
    val short = "p"
    val long = "port"
  }

  test parse "--port" :: Nil
}
