import scala.util.parsing.combinator._
import scala.collection.mutable.HashSet
import scala.io.Source
//******************************************************************************************************************
//  Parser for the CICOM language
//    - verifies if the written code is syntactically correct
//******************************************************************************************************************
class ParCiCom extends RegexParsers {
  //Define all the possible parameters and structures
  def Character: Parser[Any] = "[a-z]+".r | "[A-Z]+" .r| "?" | "_"

  def Digit: Parser[Any] = "[0-9]".r

  def Delimiter: Parser[Any] = "(" | ")" | "[" | "]" | "," | ";"

  def Operator: Parser[Any] = "+" | "-" | "~" | "*" | "/" | "!=" | ":=" | "<=" | ">=" | "=" | "<" | ">" | "&" | "|"

  def Expression: Parser[Any] = "if" ~ Expression ~ "then" ~ Expression ~ "else" ~ Expression |"let" ~ rep(Define) ~ "in" ~ Expression | "map" ~ IdList ~ "to" ~ Expression | Term ~ rep(Binop ~ Expression)

  def Term: Parser[Any] = Factor ~ rep("(" ~> ExpList <~ ")") | Unop ~ Term | Empty | int | Boolean

  def Factor: Parser[Any] = "(" ~> Expression <~ ")" | Prim | Id

  def ExpList: Parser[Any] = rep(PropExpList)

  def PropExpList: Parser[Any] = Expression ~ "," ~ PropExpList | Expression

  def IdList: Parser[Any] = rep1(PropIdList)

  def PropIdList: Parser[Any] = Id ~ "," ~ PropIdList | Id

  def Define: Parser[Any] = Id ~ ":=" ~ Expression ~ ";"

  def Empty: Parser[Any] = "null"

  def Boolean: Parser[Any] = "true" | "false"

  def Unop: Parser[Any] = Sign | "~"

  def Sign: Parser[Any] = "+" | "-"

  def Binop: Parser[Any] = Sign | "!=" | "<=" | ">=" | "*" | "/" | "=" | "<" | ">" | "&" | "|"

  def Prim: Parser[Any] = "number?" | "function?" | "list?" | "null?" | "cons?" | "cons" | "first" | "rest" | "arity"

  def int: Parser[Any] = rep1(Digit)
  
  //define reserved words
  def reservedWords = HashSet("if", "then", "map", "to", "else", "let", "in", "null")
  def idRegex= """[a-zA-Z][\.a-zA-Z0-9_-]*""".r
  def Id = Parser(input =>
    idRegex(input).filterWithError(
      !reservedWords.contains(_),
      reservedWord => s"$reservedWord is a reserved word and cannot be used as stated",
      input
    )
  )
}

//**************************************************************************************************************
//  Main Program to run parser and verify if written code is correct
//**************************************************************************************************************

object Main extends ParCiCom {
  def main(args: Array[String]): Unit = {
    // read file and retrieve string lines to parse
    val file = Source.fromFile("src/main/scala/testprogram").getLines().mkString

    //parse expressions and verify correctness
    parse(Expression, file) match {
      case Success(matched, _) => println("\nParsing Successful, No Syntactic Problems Found...\n" + matched)
      case Failure(msg, _) => println(s"Failure Syntactic Problem Found: $msg")
      case Error(msg, _) => println(s"Error: $msg")
    }

    //parse expressions and verify correctness
    val parser: ParCiCom = new ParCiCom
    val review: parser.ParseResult[Any] = parser.parseAll(parser.Expression, file)
    if (review.successful) {
      println("Parsing Successful, Program Syntactically Correct...")
    } else {
      println("Parsing failed, Program Syntactically Incorrect")
    }
    println(review.toString)

  }

}