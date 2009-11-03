def forEach[T] (f: T => Unit, items: List[T]):Unit = items match {
  case Nil =>
  case _ =>
    f (items.head)
    forEach (f, items.tail)
}

forEach ((x:Int) => println (x*x), List(1, 2, 3, 4, 5))
  
