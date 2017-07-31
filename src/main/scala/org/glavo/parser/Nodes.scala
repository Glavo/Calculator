package org.glavo.parser

import java.io.Writer

import scala.collection.mutable

trait Node {
    def eval(implicit env: Environment): BigDecimal
}

case class Number(n: BigDecimal) extends Node {
    def eval(implicit env: Environment): BigDecimal =
        n
}

case class +(l: Node, r: Node) extends Node {
    def eval(implicit env: Environment): BigDecimal =
        l.eval + r.eval
}

case class -(l: Node, r: Node) extends Node {
    def eval(implicit env: Environment): BigDecimal =
        l.eval - r.eval
}

case class *(l: Node, r: Node) extends Node {
    def eval(implicit env: Environment): BigDecimal =
        l.eval * r.eval
}

case class /(l: Node, r: Node) extends Node {
    def eval(implicit env: Environment): BigDecimal =
        l.eval / r.eval
}

case class Let(name: String, value: Node) extends Node {
    def eval(implicit env: Environment): BigDecimal = {
        env(name) = value.eval
        null
    }
}

case class Value(name: String) extends Node {
    def eval(implicit env: Environment): BigDecimal = {
        env.symbolList.getOrElse(name, null)
    }
}

case object Exit extends Node {
    override def eval(implicit env: Environment): BigDecimal = {
        throw new Error("exit!")
    }
}