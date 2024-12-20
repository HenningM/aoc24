import * as readline from 'readline';

const directions = [[0, 1], [0, -1], [1, 0], [-1, 0]];

class RaceMap {
  map: string[];
  startPoint: number[];
  endPoint: number[];
  _dmCache: { [key: string]: number } = {};

  constructor(input: string[]) {
    this.map = input;
    this.startPoint = this._findNode('S')!;
    this.endPoint = this._findNode('E')!;
  }

  _findNode(nodeType: string): number[] | undefined {
    for (let y = 0; y < this.map.length; y++) {
      for (let x = 0; x < this.map[y].length; x++) {
        if (this.map[y][x] == nodeType) {
          return [x, y];
        }
      }
    };
  }

  _isInBounds([x, y]: number[]): boolean {
    return x > -1 && x < this.map.length && y > -1 && y < this.map.length;
  }

  _isWall([x, y]: number[]): boolean {
    return this.map[y][x] == "#";
  }

  _reconstructPath(pm: { [key: string]: number[] }) {
    let par = pm[pointToStr(this.endPoint)];
    const path = [this.endPoint]
    while (par) {
      path.push(par);
      par = pm[pointToStr(par)];
    }
    path.reverse();
    return path;
  }

  _findPathToEndPoint(): number[][] | undefined {
    const openSet = [this.startPoint];
    this._dmCache[pointToStr(this.startPoint)] = 0;
    const pm: { [key: string]: number[] } = {};
    const visited: { [key: string]: boolean } = {};
    visited[pointToStr(this.startPoint)] = true;

    while (openSet.length > 0) {
      const cur = openSet.pop()!;
      const curKey = pointToStr(cur);
      visited[curKey] = true;
      for (const dir of directions) {
        const newPoint: number[] = [cur[0] + dir[0], cur[1] + dir[1]];
        const newKey = pointToStr(newPoint);
        if (this._isInBounds(newPoint) && !this._isWall(newPoint) && !visited[pointToStr(newPoint)]) {
          pm[newKey] = cur;
          this._dmCache[newKey] = this._dmCache[curKey] + 1;
          openSet.push(newPoint);
        }
      }
    }
    return this._reconstructPath(pm);
  }

  _countCheatPathsFrom(node: number[], minSavings: number, cheatCount: number): number {
    const startScore = this._dmCache[pointToStr(node)];
    const endNodes: { [key: string]: boolean } = {};

    for (let y = -cheatCount; y <= cheatCount; y++) {
      for (let x = -cheatCount; x <= cheatCount; x++) {
        const distance = Math.abs(x) + Math.abs(y);
        if (distance <= cheatCount) {
          const newPoint: number[] = [node[0] + x, node[1] + y];
          const newScore = this._dmCache[pointToStr(newPoint)];
          if (!newScore) {
            continue;
          }
          if (startScore + distance + minSavings <= newScore) {
            endNodes[pointToStr(newPoint)] = true;
          }
        }
      }
    }
    return Object.keys(endNodes).length;
  }

  countCheatPaths(minSavings: number, cheatCount: number): number {
    const honestPath = this._findPathToEndPoint()!;
    let cheatPaths = 0;

    for (const n of honestPath) {
      cheatPaths += this._countCheatPathsFrom(n, minSavings, cheatCount);
    }

    return cheatPaths;
  }
}

const pointToStr = (point: number[]) => {
  return `${point[0]},${point[1]}`;
};

const readInput = (): Promise<RaceMap> => {
  return new Promise((res) => {
    const inputLines: string[] = [];
    const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      terminal: false
    });
    rl.on('line', (line: string) => {
      inputLines.push(line);
    });

    rl.once('close', () => {
      res(new RaceMap(inputLines));
    });
  });
};

const partOne = (input: RaceMap) => {
  const cheats = input.countCheatPaths(100, 2);
  return cheats;
};

const partTwo = (input: RaceMap) => {
  const cheats = input.countCheatPaths(100, 20);
  return cheats;
};

(async function() {
  const input = await readInput();
  console.log(partOne(input));
  console.log(partTwo(input));
})();
