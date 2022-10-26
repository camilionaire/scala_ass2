


object EvalOrder {

    var flag:Boolean = true

    def daFunc(i:Unit, j:Unit):Unit = {
    }

    def leftFun():Unit = {
        if (flag) println("left-to-right")
        flag = false
    }

    def rightFun():Unit = {
        if (flag) println("right-to-left")
        flag = false
    }

    def main(argv: Array[String]) = {
        daFunc(leftFun() , rightFun())
    }
}