import sys
import heapq
from collections import defaultdict, deque

directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]

def in_bounds(p, size):
    return p[0] >= 0 and p[0] < size[0] and p[1] >= 0 and p[1] < size[1]

def dijkstra(start_node, end_node, map_size, d, dirIdx = 1):
    open_set = [(0, start_node, dirIdx)]
    parent_map = defaultdict(lambda: [])
    dist = defaultdict(lambda: float("inf"))
    dist[(start_node, dirIdx)] = 0
    best = 0
    ends = set()

    while open_set:
        score, cur, dir = heapq.heappop(open_set)

        if cur == end_node:
            ends.add((cur, dir))
            best = score
            continue

        if best != 0 and score >= best:
            continue

        left = 3 if dir == 0 else dir - 1
        right = (dir + 1) % 4

        neighbors = []
        for lr in [left, right]:
            neighbors.append((score + 1000, cur, lr))
        di = directions[dir]
        new_x = cur[0] + di[0]
        new_y = cur[1] + di[1]
        new_node = (new_x, new_y)
        if in_bounds((new_x, new_y), map_size):
            new_score = score + d(cur, new_node)
            neighbors.append((new_score, new_node, dir))
        for n in neighbors:
            score, node, di = n
            if score < dist[(node, di)]:
                dist[(node, di)] = score
                parent_map[(node, di)] = [(cur, dir)]
                heapq.heappush(open_set, (score, node, di))
            elif score == dist[(node, di)]:
                heapq.heappush(open_set, (score, node, di))
                parent_map[(node, di)].append((cur, dir))
    to_expand = deque()
    to_expand.extend(ends)
    visited = set()
    nodes = set()
    while to_expand:
        node = to_expand.popleft()
        if node[0] not in nodes:
            nodes.add(node[0])
        if node not in visited:
            to_expand.extend(parent_map[node])
            visited.add(node)
    return best, len(nodes)

def read_input():
    lines = []
    for line in sys.stdin:
        lines.append(line.strip())
    return lines

def find_node(input, char):
    y = 0
    for line in input:
        x = line.find(char)
        if x != -1:
            return (x, y)
        y += 1
    return False

def make_calc_distance(input):
    def calc_distance(start_node, next_node):
        next_char = input[next_node[1]][next_node[0]]
        if next_char == "#":
            return float("inf")
        score = abs(next_node[0] - start_node[0]) + abs(next_node[1] - start_node[1])
        return score
    return calc_distance

input = read_input()
input_size = (len(input[0]), len(input))
start_node = find_node(input, "S")
end_node = find_node(input, "E")

best, node_count = dijkstra(start_node, end_node, input_size, make_calc_distance(input))

print(best)
print(node_count)
