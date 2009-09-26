// k-term finite continued fraction

def contFrac (n: (Int) => Double, d: (Int) => Double, k: Int) = {
  def cf (i: Int): Double = {
    if (i == k)
      n(i) / d(i)
    else 
      n(i) / (d(i) + cf(i+1))
  }
  cf(1)
}
    
def contFracIter (n: (Int) => Double, d: (Int) => Double, k: Int) = {
  // start at k and work backwards
  def cfIter (i: Int, accum: Double): Double = {
    if (i == 1) 
      n(i) / (d(i) + accum)
    else
      cfIter(i - 1, n(i) / (d(i) + accum))
  }
  cfIter(k, 0.0)
}

val one: (Int) => Double = (n: Int) => 1.0
def calcInversePhi (n: Int) = contFrac(one, one, n)
def calcInversePhiIter (n: Int) = contFracIter(one, one, n)
  
val inversePhi = 0.6180339882723972
val inversePhiTo4Places = 0.6180

def roundTo4Places (x: Double) = Math.round(x * 10000.0) / 10000.0
assert (roundTo4Places(inversePhi) == inversePhiTo4Places)

println("Inverse of phi: " + inversePhi)

def tryPhiCalc (n: Int, calc: (Int) => Double): Unit = {
  if (roundTo4Places(calc(n)) == inversePhiTo4Places)
    println(n)
  else 
    tryPhiCalc(n + 1, calc)
}

tryPhiCalc(5, (n:Int) => calcInversePhi(n)) // returns 10

println("n=9:   " + calcInversePhi(9))   // 0.6181818181818182
println("n=10:  " + calcInversePhi(10))  // 0.6179775280898876  - rounds to correct 4 decimal places
println("n=11:  " + calcInversePhi(11))  // 0.6180555555555556
println("n=12:  " + calcInversePhi(12))  // 0.6180257510729613
println("n=13:  " + calcInversePhi(13))  // 0.6180371352785146
println("n=100: " + calcInversePhi(100)) // 0.6180339887498948

tryPhiCalc(5, (n:Int) => calcInversePhiIter(n)) // returns 10

println("n=9:   " + calcInversePhiIter(9))   // 0.6181818181818182
println("n=10:  " + calcInversePhiIter(10))  // 0.6179775280898876  - rounds to correct 4 decimal places
println("n=11:  " + calcInversePhiIter(11))  // 0.6180555555555556
println("n=12:  " + calcInversePhiIter(12))  // 0.6180257510729613
println("n=13:  " + calcInversePhiIter(13))  // 0.6180371352785146
println("n=100: " + calcInversePhiIter(100)) // 0.6180339887498948

