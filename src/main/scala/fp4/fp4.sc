// 1. 方法和函数
def aMethod(x:Int):Int = x + 10

// 函数本身是可以返回自身的表达式；方法不是表达式
val aFunction = (x:Int) => x + 10

// 方法转换成函数

val bFunction = aMethod _

// Scala的编译器能针对需要函数的地方把方法转换成函数

// 函数其实就是普通的对象，跟 Java 8 类似，下面是等价的
val addThem = (a: Int, b: Int) => a + b
// 编译器会处理成下面的实现
val addThem2 = new Function2[Int, Int, Int] {
  def apply(a:Int, b:Int) = a + b
}

// 2. 多态函数

def findFirstInt(arr: Array[Int], target: Int): Int = {
  def loop(idx: Int): Int = idx match {
    case l if (l >= arr.length) => -1          //indicate not found
    case i if (arr(i) == target) => idx
    case _ => loop(idx + 1)
  }
  loop(0)
}                                               //> findFirst: (arr: Array[Int], target: Int)Int
findFirstInt(Array(2,4,3,9,0),3)                //> res53: Int = 2
findFirstInt(Array(2,4,3,9,0),7)                //> res54: Int = -1

def findFirstString(arr: Array[String], target: String): Int = {
  def loop(idx: Int): Int = idx match {
    case l if (l >= arr.length) => -1        //indicate not found
    case i if (arr(i) == target) => idx
    case _ => loop(idx + 1)
  }
  loop(0)
}                                               //> findFirstString: (arr: Array[String], target: String)Int
findFirstString(Array("Hello","My","World"),"My")
//> res55: Int = 1
findFirstString(Array("Hello","My","World"),"Yours")
//> res56: Int = -1

// 上面两个函数可以抽象出一个

def findFirstA[A](arr: Array[A],target: A)(equ: (A,A) => Boolean): Int = {
  def loop(idx: Int): Int = idx match {
    case l if (l >= arr.length) => -1    //indicate not found
    case i if (equ(arr(i),target)) => idx
    case _ => loop(idx + 1)
  }
  loop(0)

}                                           //> findFirstA: [A](arr: Array[A], target: A)(equ: (A, A) => Boolean)Int
findFirstA[Int](Array(2,4,3,9,0),3)((x,y) => x == y)
//> res57: Int = 2
findFirstA[String](Array("Hello","My","World"),"My")((x,y) => x == y)
//> res58: Int = 1

// compose 和 andThen 顺序上是相反的