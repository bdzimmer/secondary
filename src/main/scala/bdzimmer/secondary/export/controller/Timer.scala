

package bdzimmer.secondary.export.controller

object Timer {

    def showTime[T](name: String, expression: => T): T = {
        println("~~~~ " + name + " ~~~~")
        Console.flush()
        val (totalTime, res) = timeit(expression)
        println("~~~~ " + name + ": " + totalTime + " sec ~~~~")
        Console.flush()
        res
    }

    def showTimeBrief[T](name: String, expression: => T): T = {
        print(name + "...")
        Console.flush()
        val (totalTime, res) = timeit(expression)
        println("done\t" + totalTime + " sec")
        Console.flush()
        res
    }

    def timeit[T](expression: => T): (Double, T) = {
        val startTime = System.currentTimeMillis
        val res = expression // evaluate the expression
        val totalTime = (System.currentTimeMillis - startTime) / 1000.0
        (totalTime, res)
    }

}
