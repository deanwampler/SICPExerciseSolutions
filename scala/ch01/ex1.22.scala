// Some modifications to support testing, rather than just printing the result, 
// although the testing isn't very useful in this case. 
// Wrote this one a little different than the Clojure and Scheme versions, too.

def square(n: Int) = n * n

def findDivisor(n: Int, testDivisor: Int): Int = {
  if (square(testDivisor) > n) n
  else if (divides(testDivisor, n)) testDivisor
  else findDivisor(n, testDivisor + 1)
}

def smallestDivisor(n: Int) = findDivisor(n, 2)

def divides(testDivisor: Int, n: Int) = n % testDivisor == 0

def prime(n: Int) = smallestDivisor(n) == n

def now = System.currentTimeMillis

def timedPrimeTest (n: Int) = {
  startPrimeTest (n, now)
}

def reportPrime(n: Int, isPrime: Boolean, elapsedTime: Long) = 
  format("%d: prime? %b, time: %d\n", n, isPrime, elapsedTime)
  
def startPrimeTest (n: Int, startTime: Long) = {
  val isPrime = prime (n)  // calculate separately so we elapse time before next call.
  reportPrime(n, isPrime, now - startTime)
}
    
def searchForPrimes(start: Int, end: Int): Unit = {
  if (start < end) {
    timedPrimeTest(start)
    searchForPrimes(start + 2, end)
  }
}

// Find a range of primes.
searchForPrimes(1001, 1101)
searchForPrimes(10001, 10101)
searchForPrimes(100001, 100101)
searchForPrimes(1000001, 1000101)

// Run 4 non-primes again, to see the times:
startPrimeTest(1007, now)
startPrimeTest(10059, now)
startPrimeTest(100067, now)
startPrimeTest(1000001, now)

// Run 12 primes again, to see the times:
val savetime = now
startPrimeTest(1009, now)
startPrimeTest(1013, now)
startPrimeTest(1019, now)
startPrimeTest(10007, now)
startPrimeTest(10009, now)
startPrimeTest(10037, now)
startPrimeTest(100003, now)
startPrimeTest(100019, now)
startPrimeTest(100033, now)
startPrimeTest(1000003, now)
startPrimeTest(1000033, now)
startPrimeTest(1000037, now)
// Times are all reported as zero! Is it really that fast? Maybe...
format("%dl, %dl", now, now - savetime)