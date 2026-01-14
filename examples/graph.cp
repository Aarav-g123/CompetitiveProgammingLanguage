# BFS on a graph
n: int = 0
m: int = 0
read(n, m)

adj: vector<vector<int>> = [[]]

for i in range(m):
    u: int = 0
    v:  int = 0
    read(u, v)
    adj[u].push(v)
    adj[v]. push(u)

visited: vector<bool> = [False] * n
dist: vector<int> = [-1] * n

# BFS from node 0
q: deque<int> = []
q.push(0)
dist[0] = 0
visited[0] = True

while not empty(q):
    u: int = q.front()
    q.pop_front()
    for v in adj[u]:
        if not visited[v]: 
            visited[v] = True
            dist[v] = dist[u] + 1
            q. push(v)

for i in range(n):
    print(dist[i])