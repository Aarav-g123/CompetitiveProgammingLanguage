# Fibonacci sequence
def fib(n:  int) -> int:
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

n: int = 10
for i in range(n):
    print(fib(i))