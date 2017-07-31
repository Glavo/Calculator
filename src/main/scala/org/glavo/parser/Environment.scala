package org.glavo.parser

import scala.collection.mutable


/**
  * Created by Glavo on 17-7-31.
  *
  * @author Glavo
  * @since 0.1.0
  */
class Environment(val symbolList: mutable.Map[String, BigDecimal] = mutable.HashMap()) {
    symbolList("let") = null

    def apply(key: String): BigDecimal =
        symbolList(key)

    def update(key: String, value: BigDecimal): Unit =
        symbolList(key) = value

    def exit: Unit =
        throw new Error()
}
