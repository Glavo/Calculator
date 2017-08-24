package org.glavo.parser

import jline.console._

object Calculator extends CalculatorParser {
    def main(args: Array[String]): Unit = {
        implicit val env: Environment =
            new Environment()

        val reader = new ConsoleReader()

        reader.setHistoryEnabled(true)

        reader.addCompleter(new Completer(env.symbolList.keySet))

        try {
            while (true) {
                val input = reader.readLine("Calculator> ").trim

                val ans = try {
                    parseAll(grammar, input)
                } catch {
                    case t: Throwable =>
                        System.err.println(s"Error: ${t.getMessage}")
                        System.err.println()
                        null
                }


                if (ans != null)
                    ans match {
                        case Success(matched, _) =>
                            matched match {
                                case l @ Let(name, value) =>
                                    l.eval
                                    reader.println(s"$name = ${value.eval}")
                                    reader.println()

                                case other =>
                                    val v = other.eval
                                    reader.println("-> " + (if(v == null) "()" else v.toString()))
                                    reader.println()
                            }

                        case Failure(msg, _) =>
                            reader.println("FAILURE: " + msg)
                            reader.println()

                        case Error(msg, _) =>
                            reader.println("ERROR: " + msg)
                            reader.println()

                    }
            }

        } catch {
            case _ : Throwable =>
                reader.println("Leaving Calculator.")
        } finally {
            reader.close()
        }

    }
}
