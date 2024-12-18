use std::collections::HashSet;
use std::iter::FromIterator;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::cmp::Reverse;
use std::io::{self};

type Point = (i32, i32);

const DIRECTIONS: &'static [(i32, i32)] = &[
    (0, 1),
    (0, -1),
    (1, 0),
    (-1, 0)
];

#[derive(Debug, Clone)]
struct Grid {
    start_point: Point,
    end_point: Point,
    walls: HashSet<Point>,
    size: Point,
}

impl Grid {
    pub fn new(walls: Vec<Point>) -> Self {
        let end_point;
        let start_point = (0, 0);
        let wall_set;
        let size;
        if walls.len() < 30 {
            end_point = (6, 6);
            size = (7, 7);
            wall_set = HashSet::from_iter(walls.iter().cloned());
        } else {
            end_point = (70, 70);
            size = (71, 71);
            wall_set = HashSet::from_iter(walls.iter().cloned());
        }
        Self {
            start_point,
            end_point,
            walls: wall_set,
            size
        }
    }

    pub fn point_in_bounds(&self, point: Point) -> bool {
        point.0 > -1 && point.0 < self.size.0 && point.1 > -1 && point.1 < self.size.1
    }
}

fn reconstruct_path(parent_map: HashMap<Point, Point>, cur: Point) -> Vec<Point> {
    let mut c = cur;
    let mut path = vec![c];
    while parent_map.contains_key(&c) {
        c = *parent_map.get(&c).unwrap();
        path.insert(0, c);
    }
    path
}

fn astar_path(grid: Grid, h: impl Fn(Point) -> i32, d: impl Fn(Point, Point) -> i32) -> Vec<Point> {
    let mut open_nodes = BinaryHeap::new();
    let mut open_nodes_set = HashSet::from([grid.start_point]);
    open_nodes.push(Reverse((0, grid.start_point)));

    let mut parent_map = HashMap::new();

    let mut g_score = HashMap::new();
    for x in 0..grid.size.0 {
        for y in 0..grid.size.1 {
            g_score.insert((x, y), i32::MAX);
        }
    }
    g_score.insert(grid.start_point, 0);

    let mut f_score = HashMap::new();
    for x in 0..grid.size.0 {
        for y in 0..grid.size.1 {
            f_score.insert((x, y), i32::MAX);
        }
    }
    f_score.insert(grid.start_point, h(grid.start_point));

    while open_nodes.len() > 0 {
        let (_, cur) = open_nodes.pop().unwrap().0;
        open_nodes_set.remove(&cur);

        if cur == grid.end_point {
            return reconstruct_path(parent_map, cur);
        }

        let neighbors = DIRECTIONS.iter().map(|d| (cur.0 + d.0, cur.1 + d.1)).filter(|&n| grid.point_in_bounds(n) && !grid.walls.contains(&n));
        for n in neighbors {
            let tentative_g_score = g_score.get(&cur).unwrap() + d(cur, n);
            if tentative_g_score < *g_score.get(&n).unwrap() {
                parent_map.insert(n, cur);
                g_score.insert(n, tentative_g_score);
                f_score.insert(n, tentative_g_score + h(n));
                if !open_nodes_set.contains(&n) {
                    open_nodes.push(Reverse((tentative_g_score, n)));
                    open_nodes_set.insert(n);
                }
            }
        }
    }

    return vec![];
}

fn parse_input_line(line: String) -> Point {
    let mut components = line.split(",");
    let x: i32 = components.next().unwrap().parse().unwrap();
    let y: i32 = components.next().unwrap().parse().unwrap();
    (x, y)
}

fn read_input() -> Vec<Point> {
    let mut input;
    let mut lines = vec![];

    loop {
        input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => break,
            Ok(_) => {
                let coords = parse_input_line(input.trim().to_string());
                lines.push(coords);
            }
            Err(_e) => break
        }
    }
    lines
}

fn min_dist_to(point: Point) -> impl Fn(Point) -> i32 {
    move |cmp: Point| (point.0 - cmp.0).abs() + (point.1 - cmp.1).abs()
}

fn dist_between(a: Point, b: Point) -> i32 {
    (a.0 - b.0).abs() + (a.1 - b.1).abs()
}

fn part_one(coords: Vec<Point>) {
    let walls;
    if coords.len() < 30 {
        walls = coords[..12].to_vec();
    } else {
        walls = coords[..1024].to_vec();
    }
    let grid = Grid::new(walls);
    let path = astar_path(grid.clone(), min_dist_to(grid.end_point), dist_between);
    println!("{}", path.len() - 1);
}

fn part_two(coords: Vec<Point>) {
    let mut path;
    let mut low = if coords.len() > 30 { 1024 } else { 1 };
    let mut high = coords.len() - 1;
    while high - low > 1 {
        let i = (low + high) / 2;
        let walls = coords[..i].to_vec();
        let grid = Grid::new(walls);
        path = astar_path(grid.clone(), min_dist_to(grid.end_point), dist_between);
        if path.len() == 0 {
            high = i;
        } else {
            low = i;
        }
    }
    let (x, y) = coords.iter().nth(high-1).unwrap();
    println!("{},{}", x, y);
}

fn main() {
    let input = read_input();
    part_one(input.clone());
    part_two(input.clone());
}

