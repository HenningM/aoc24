package main

import "fmt"
import "bufio"
import "os"
import "strings"
import "strconv"
import "math"

type Equation struct {
	test int
	nums []int
}

func (e *Equation) isValid() bool {
	var numOps = len(e.nums) - 1
	var possibleCombs = math.Pow(2, float64(numOps))
	for i := range int(possibleCombs) {
		var sum = e.nums[0]
		for j, v := range e.nums[1:] {
			var op = i >> j & 1
			if op == 1 {
				sum *= v
			} else {
				sum += v
			}
		}
		if sum == e.test {
			return true
		}
	}
	return false
}

func (e *Equation) isValidV2() bool {
	var numOps = len(e.nums) - 1
	var possibleCombs = math.Pow(3, float64(numOps))
	for i := range int(possibleCombs) {
		var sum = e.nums[0]
		for j, v := range e.nums[1:] {
			var op = i / int(math.Pow(3, float64(j))) % 3
			if op == 2 {
				var str = fmt.Sprintf("%d%d", sum, v)
				var num, _ = strconv.Atoi(str)
				sum = num
			} else if op == 1 {
				sum *= v
			} else {
				sum += v
			}
			if sum > e.test {
				break
			}
		}
		if sum == e.test {
			return true
		}
	}
	return false
}

func parseEquation(inp string) Equation {
	var parts = strings.Split(inp, ": ")
	var test, _ = strconv.Atoi(parts[0])
	var nums []int

	for _, n := range strings.Split(parts[1], " ") {
		var ni, _ = strconv.Atoi(n)
		nums = append(nums, ni)
	}
	return Equation { test: test, nums: nums }
}

func readInput() []Equation {
	var equations []Equation
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		var equation = parseEquation(scanner.Text())
		equations = append(equations, equation)
	}
	return equations
}

func main() {
	var total int
	var totalv2 int

	for _, v := range readInput() {
		if v.isValid() {
			total += v.test
		}
		if v.isValidV2() {
			totalv2 += v.test
		}
	}
	fmt.Println(total)
	fmt.Println(totalv2)
}
