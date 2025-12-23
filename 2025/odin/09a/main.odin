package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:slice"

Connection :: struct {
    distance: f64,
    boxes: [2]int
}

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


    total_sum := 0

    red_tiles := make([dynamic][2]int)

    line_nbr := 0
	for line in strings.split_lines_iterator(&it) {
        splits := strings.split(line, ",")

        left, _ := strconv.parse_int(splits[0])
        right, _ := strconv.parse_int(splits[1])

        append(&red_tiles, [2]int{left, right})

        line_nbr += 1
    }
    current_max := 0
    for t1, i1 in red_tiles[:len(red_tiles)-1] {
        for t2, i2 in red_tiles[i1+1:] {
            length := math.abs(t1.x - t2.x) + 1
            height := math.abs(t1.y - t2.y) + 1 

            current_max = max(current_max, length * height)
            fmt.println(t1, t2, length, height, current_max)
        
        }
    }

    total_sum = current_max

    fmt.println(total_sum)
}
