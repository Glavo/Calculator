package org.glavo.parser

import java.util

import jline.internal.Preconditions.checkNotNull

import scala.collection.mutable
import scala.util.control.Breaks


class Completer(val strings: scala.collection.Set[String]) // empty
    extends jline.console.completer.Completer {

    def this(ss: collection.Seq[String]) {
        this(ss.toSet)
    }

    override def complete(buffer: String, cursor: Int, candidates: util.List[CharSequence]): Int = { // buffer could be null
        checkNotNull(candidates)

        val strings = mutable.TreeSet[String]()

        strings ++= this.strings

        if (buffer == null)
            strings.foreach(it => candidates.add(it))
        else {
            for (m <- strings if m.startsWith(buffer)) {
                if (!m.startsWith(buffer)) Breaks.break()
                candidates.add(m)
            }
        }

        if (candidates.isEmpty) -1
        else 0
    }
}