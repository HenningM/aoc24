import os

fn read_input() []u64 {
    mut numbers := []u64{}
    for {
        line := os.get_line()
        if line == '' {
            break
        }
        numbers << u64(line.int())
    }
    return numbers
}

fn prune(x u64) u64 {
    return x % 16777216
}

fn calc_secret_after_iterations(x u64, iterations int) u64 {
    mut r := x
    for _ in 0..iterations {
        r = calc_next_secret(r)
    }
    return r
}

fn calc_next_secret(x u64) u64 {
    mut r := x
    r = prune(r ^ (r * 64))
    r = prune(r ^ (r / 32))
    r = prune(r ^ (r * 2048))
    return r
}

fn solve_part_one(input []u64) u64 {
    mut sum := u64(0)
    for x in input {
        num := calc_secret_after_iterations(x, 2000)
        sum += num
    }
    return sum
}

fn solve_part_two(input []u64) u64 {
    mut sequences := map[string]u64{}
    for x in input {
        mut s := x
        mut prev := x % 10
        mut deltas := []string{}
        mut visited := map[string]bool

        for _ in 0..2000 {
            s = calc_next_secret(s)
            price := s % 10
            delta := price - prev
            prev = price
            deltas = deltas#[-3..].clone()
            deltas << delta.str()

            if deltas.len > 3 {
                delta_key := deltas.join(",")
                if !(delta_key in visited) {
                    visited[delta_key] = true
                    sequences[delta_key] += price
                }
            }
        }
    }

    mut high := u64(0)
    for _, v in sequences {
        if v > high {
            high = v
        }
    }
    return high
}

fn main() {
    input := read_input()

    p1 := solve_part_one(input)
    println("${p1}")

    p2 := solve_part_two(input)
    println("${p2}")
}
