val f = (g: (Int) => Int) => g(2)

val square = (n:Int) => n * n

println(f(square))
println(f(x => x * (1+x)))
// Won't compile:
//f(f)