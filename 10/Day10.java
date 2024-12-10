import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;
import java.util.Arrays;
import java.util.Objects;
import java.util.Set;
import java.util.HashSet;
import java.util.stream.Collectors;

enum Direction {
    NORTH(0, -1),
    EAST(1, 0),
    SOUTH(0, 1),
    WEST(-1, 0);

    int x;
    int y;

    private Direction(int x, int y) {
        this.x = x;
        this.y = y;
    }
}

record Point(int x, int y) {
    public int countPaths(int[][] map, int prevVal) {
        if (!this.isInBounds(map)) {
            return 0;
        }
        var curValue = map[y][x];
        if (curValue - prevVal != 1) {
            return 0;
        }
        if (curValue == 9) {
            return 1;
        }
        var sum = 0;
        for (Direction d: Direction.values()) {
            var point = new Point(this.x + d.x, this.y + d.y);
            sum += point.countPaths(map, curValue);
        }
        return sum;
    }

    public Set<Point> findReachableTops(int[][] map, int prevVal) {
        if (!this.isInBounds(map)) {
            return Set.of();
        }
        var curValue = map[y][x];
        if (curValue - prevVal != 1) {
            return Set.of();
        }
        if (curValue == 9) {
            return Set.of(this);
        }
        var tops = new HashSet<Point>();
        for (Direction d: Direction.values()) {
            var point = new Point(this.x + d.x, this.y + d.y);
            tops.addAll(point.findReachableTops(map, curValue));
        }
        return tops;
    }

    boolean isInBounds(int[][] map) {
        return (this.x > -1 && this.x < map.length) && (this.y > -1 && this.y < map.length);
    }
}

class Day10
{
    static int[][] readInput()
    {
        var input = new Scanner(System.in);
        var parsed = new ArrayList<int[]>();
        while (input.hasNext()) {
            var line = input.nextLine();
            var split = line.split("");
            var nums = Arrays.asList(split).stream().mapToInt(Integer::parseInt).toArray();
            parsed.add(nums);
        }
        return parsed.toArray(new int[parsed.size()][parsed.size()]);
    }

    static List<Point> findTrailHeads(int[][] input) {
        var trailHeads = new ArrayList<Point>();
        for (int y = 0; y < input.length; y++) {
            for (int x = 0; x < input[y].length; x++) {
                if (input[y][x] == 0) {
                    trailHeads.add(new Point(x, y));
                }
            }
        }
        return trailHeads;
    }

    public static void main(String []args)
    {
        var input = readInput();
        var trailHeads = findTrailHeads(input);
        var sum = 0;
        for (Point i: trailHeads) {
            var tops = i.findReachableTops(input, -1);
            sum += tops.size();
        }
        System.out.println(sum);
        var sum2 = 0;
        for (Point i: trailHeads) {
            var tops = i.countPaths(input, -1);
            sum2 += tops;
        }
        System.out.println(sum2);
    }
};
