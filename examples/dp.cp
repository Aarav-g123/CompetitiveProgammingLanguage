# Dynamic Programming - Longest Increasing Subsequence
n: int = 0
read(n)

arr: vector<int> = []
for i in range(n):
    x: int = 0
    read(x)
    arr.push(x)

# O(n log n) LIS
lis: vector<int> = []
for i in range(n):
    pos: auto = lower_bound(lis, arr[i])
    if pos == len(lis):
        lis.push(arr[i])
    else:
        lis[pos] = arr[i]

print(len(lis))