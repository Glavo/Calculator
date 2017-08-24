package org.glavo.parser

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by Glavo on 17-7-31.
  *
  * @author Glavo
  * @since 0.1.0
  */
class CalculatorParser extends JavaTokenParsers {
    def number: Parser[Node] =
        """-?[0-9]+(\.[0-9]+)?""".r ^^ { it => Number(BigDecimal(it)) } |
            value

    def expr: Parser[Node] =
        term ~ (("+" | "-") ~ term).* ^^ {
            case t ~ list => list.foldLeft(t) { (n, t) =>
                t match {
                    case ("+" ~ nn) => new +(n, nn)
                    case ("-" ~ nn) => new -(n, nn)
                }
            }
        }


    def term: Parser[Node] =
            atom ~ (("*" | "/") ~ atom).* ^^ {
                case t ~ list => list.foldLeft(t) { (n, t) =>
                    t match {
                        case ("*" ~ nn) => *(n, nn)
                        case ("/" ~ nn) => /(n, nn)
                    }
                }
            }

    def atom: Parser[Node] =
        number |
            "(" ~> expr <~ ")"

    def value: Parser[Node] =
        ident ^^ Value

    def statement: Parser[Node] =
        let

    def let: Parser[Node] =
        "let".r ~ ident ~ "=" ~ expr ^^ {
            case _ ~ name ~ _ ~ value => Let(name, value)
        }


    def grammar: Parser[Node] =
        statement | expr
}
