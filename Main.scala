import scala.util.parsing.combinator._

abstract class Tree
case class Var(n: String) extends Tree
case class Alternation(al: Tree, ar: Tree) extends Tree
case class Optional(o: Tree) extends Tree
case class Parenthesis(m: Tree) extends Tree
case class Concatination(z: Tree, y: Tree) extends Tree


class Combinators extends JavaTokenParsers {

  override def skipWhitespace: Boolean = false

  def E: Parser[Tree] = T ~ alternation ~ E  ^^ {case l ~ p ~ r => Alternation(l,r)} | T
  def S: Parser[Tree] = E
  def T: Parser[Tree] = F ~ T ^^ {case l ~ r => {Concatination(l,r)}} | F
  def F: Parser[Tree] = A ~ optional ^^ {case  l ~ r => Optional(l)} | A
  def A: Parser[Tree] = C | parenthesisL ~ E ~ parenthesisR ^^ {case l ~ w ~ r => Parenthesis(w)}
  def C: Parser[Tree] = varname

  def varname: Parser[Var] = "[a-zA-Z0-9_ /.]".r ^^ { str => Var(str) }
  def alternation[Tree] = "|"
  def optional[Tree]= "?"
  def parenthesisL[Tree] = "("
  def parenthesisR[Tree] = ")"

}

object Main extends Combinators {
  var startIndex = 0
  var endIndex = 1
  var counter = 0
  var oldStart = 0
  var sentence: String = ""
  override def skipWhitespace: Boolean = false


  def eval(t: Tree, env: String): Boolean = t match {
    case Alternation(al, ar) => {
      oldStart = startIndex;
      endIndex = oldStart + 1;
      counter = startIndex;
      if(eval(al,env)) true
      else {
        startIndex = oldStart;
        endIndex = startIndex +1;
        counter = startIndex
        if(eval(ar,env)) true else false
      }
    }

    case Optional(o) => {
      oldStart = startIndex
      endIndex = startIndex+1;
      counter = startIndex;
      if(eval(o,env)) true else {
        if(counter > env.length || startIndex > env.length) false else{
        startIndex = oldStart;
        endIndex = startIndex+1;
        counter = startIndex;
        true
        }
      }
    }

    case Parenthesis(p) =>
      if(eval(p,env)) {
      oldStart = startIndex;
      endIndex = oldStart + 1;
      true
    }
    else false

    case Concatination(y,z) => if(eval(y,env) && eval(z,env)) true else false

    case Var(n) if startIndex >= env.length => false
    case Var(n) if n == "." && env.substring(startIndex,endIndex) != null => {
      //println(n + " and "+ env.substring(startIndex,endIndex));
      increment;
      true
    }

    case Var(n) if n == env.substring(startIndex,endIndex) => {
      //println(n + " and "+ env.substring(startIndex,endIndex));
      increment;
      true
    }

    case Var(n) if n != env.substring(startIndex,endIndex) => {
      //println(n + " and "+ env.substring(startIndex,endIndex));
      false
    }
  }

  def increment(){
    startIndex += 1;
    endIndex += 1;
    counter += 1
  }

  def reset(){
  startIndex = 0
    endIndex = 1
  }

  def main(args: Array[String]){
    var pattern: String = scala.io.StdIn.readLine("pattern? ")
    sentence = scala.io.StdIn.readLine("string? " )
    val exp: Tree = parseAll(S, pattern).get
    //println(exp)
    //println(pattern)
       while(!sentence.equalsIgnoreCase("exit")){
      if(sentence != "exit")
        if(eval(exp,sentence)) {
          if(counter != sentence.length)println("no match") else println("match")
        }
        else println("not match")
      sentence = scala.io.StdIn.readLine("string? ")
         reset
    }
  }
}


//Extra Credit a(bc)?(de)?f  1.1 * Score