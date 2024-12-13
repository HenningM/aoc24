local directions = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}}

function parseLine(line)
  local chars = {}
  for c in line:gmatch"." do
    table.insert(chars, c)
  end
  return chars
end

function readInput()
  local lines = {}

  while true do
    line = io.read()
    if line == nil then
      break
    end
    parsed = parseLine(line)
    table.insert(lines, parsed)
  end
  return lines
end

function isInBounds(point, map)
  return point[2] >= 1 and point[2] <= #map and point[1] >= 1 and point[1] <= #map[1]
end

function pointFromStr(str)
  local spl = split(str, ",")
  local x = tonumber(spl[1])
  local y = tonumber(spl[2])
  return {x, y}
end

function pointFromDirStr(str)
  local spl = split(str, "d")
  local p = pointFromStr(spl[1])
  local dir = tonumber(spl[2])
  return {p, dir}
end

function getPointId(point)
  return point[1] .. "," .. point[2]
end

function getEdgeId(point, dirIdx)
  return getPointId(point) .. "d" .. dirIdx
end

function mergeTables(table1, table2)
  local newTable = {}
  for k,v in pairs(table2) do newTable[k] = v end
  for k,v in pairs(table1) do newTable[k] = v end
  return newTable
end

function expandCluster(startPoint, map, cluster)
  cur = map[startPoint[2]][startPoint[1]]
  cluster[getPointId(startPoint)] = true
  for k, d in pairs(directions) do
    local x = startPoint[1] + d[1]
    local y = startPoint[2] + d[2]
    if isInBounds({x, y}, map) then
      local id = getPointId({x, y})
      if map[y][x] == cur and cluster[id] == nil then
        expandCluster({x, y}, map, cluster)
      end
    end
  end
  return cluster
end

function findClusters(map)
  local clusters = {}
  local visited = {}
  for y, line in pairs(map) do
    for x, c in pairs(line) do
      local id = getPointId({x, y})
      if visited[id] == nil then
        local cluster = expandCluster({x, y}, map, {})
        table.insert(clusters, cluster)
        visited = mergeTables(visited, cluster)
      end
    end
  end
  return clusters
end

function calculatePerimeter(cluster)
  edges = 0
  for k, v in pairs(cluster) do
    local point = pointFromStr(k)
    for k2, d in pairs(directions) do
      local x = point[1] + d[1]
      local y = point[2] + d[2]
      local id = getPointId({x, y})
      if cluster[id] == nil then
        edges = edges + 1
      end
    end
  end
  return edges
end

function countEntries(tbl)
  local count = 0
  for _ in pairs(tbl) do count = count + 1 end
  return count
end

function getFirstTableKey(tbl)
  local first = nil
  for k, v in pairs(tbl) do
    if first == nil or first > k then
      first = k
    end
  end
  return first
end

function countNeighbors(startPoint, neighborhood, dirs, visited)
  visited[startPoint] = true
  local pointAndDir = pointFromDirStr(startPoint)
  local p = pointAndDir[1]
  local dir = pointAndDir[2]
  local neighborCount = 0
  for k, d in pairs(dirs) do
    x = p[1] + d[1]
    y = p[2] + d[2]
    nId = getEdgeId({x, y}, dir)
    if neighborhood[nId] ~= nil and visited[nId] == nil then
      neighborCount = neighborCount + 1 + countNeighbors(nId, neighborhood, dirs, visited)
    end
  end
  return neighborCount
end

function calculateDiscount(neighborhood)
  local visited = {}
  local neighbors = 0
  for k, n in pairs(neighborhood) do
    if visited[k] == nil then
      neighbors = neighbors + countNeighbors(k, neighborhood, directions, visited)
    end
  end
  return neighbors
end

function calculatePerimeterWithDiscount(cluster)
  local edgeNeighborhood = {}
  for k, v in pairs(cluster) do
    local point = pointFromStr(k)
    for k2, d in pairs(directions) do
      local x = point[1] + d[1]
      local y = point[2] + d[2]
      local id = getPointId({x, y})
      if cluster[id] == nil then
        edgeId = getEdgeId({x, y}, k2)
        if edgeNeighborhood[edgeId] == nil then
          edgeNeighborhood[edgeId] = 1
        else
          edgeNeighborhood[edgeId] = edgeNeighborhood[edgeId] + 1
        end
      end
    end
  end
  return calculatePerimeter(cluster) - calculateDiscount(edgeNeighborhood)
end

function calculatePrice(cluster)
  local perim = calculatePerimeter(cluster)
  local area = countEntries(cluster)
  return perim * area
end

function calculatePriceWithDiscount(cluster)
  local firstKey = getFirstTableKey(cluster)
  local startPoint = pointFromStr(firstKey)
  local perim = calculatePerimeterWithDiscount(cluster)
  local area = countEntries(cluster)
  return perim * area
end

function calculateFencingPrice(map, calculatePriceFunc)
  local clusters = findClusters(map)
  local sum = 0
  for k, c in pairs(clusters) do
    sum = sum + calculatePriceFunc(c)
  end
  return sum
end

function split(str, sep)
  if sep == nil then
    return {str}
  end
  local parts = {}
  for match in string.gmatch(str, "([^"..sep.."]+)") do
    table.insert(parts, match)
  end
  return parts
end

local input = readInput()

local totalPrice = calculateFencingPrice(input, calculatePrice)
print(totalPrice)

local totalPriceWithDiscount = calculateFencingPrice(input, calculatePriceWithDiscount)
print(totalPriceWithDiscount)
