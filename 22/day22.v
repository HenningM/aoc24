import os

fn main() {
    for {
        line := os.get_line()
        if line == '' {
            break
        }
        println(line)
    }
}
