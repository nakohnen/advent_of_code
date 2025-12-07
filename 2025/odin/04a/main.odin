package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"

main :: proc() {
    if len(os.args) - 1 != 1 {
        fmt.println("Need exactly one argument with the filename")
        return
    }
    filepath := os.args[1]

    data, ok := os.read_entire_file(filepath, context.allocator)
	if !ok {
        fmt.println("Could not read file.")
		return
	}
	defer delete(data, context.allocator)


	it := string(data)

    sum_target := 0

    width := 0
    height := 0

    Key_Type :: [2]int

    grid := make(map[Key_Type]int)
    rolls := make(map[Key_Type]bool)

    j := 0
	for line in strings.split_lines_iterator(&it) {
        if j > height {
            height = j
        }
        for r, i in line {
            if i > width {
                width = i
            }
            if r == '@' {
                rolls[{i, j}] = true
                // Do stuff here
                for x in i-1..=i+1 {
                    for y in j-1..=j+1 {
                        if x == i && y == j {
                            continue
                        }
                        grid[{x, y}] += 1
                    }
                }
            }
        }

        j += 1
	}
    height += 1 // Fix off by one error
    width += 1 

    for x in 0..<width {
        for y in 0..<height {
            key := Key_Type{x, y}
            val := grid[key]
            if val < 4 && rolls[key]{
                fmt.println(key, val)
                sum_target += 1
            }
        }
    }
    fmt.println(sum_target)
}
