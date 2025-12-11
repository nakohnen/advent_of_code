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

    fresh_ranges := make([dynamic][2]int)
    items := make([dynamic]int)

    ranges_mode := true
	for line in strings.split_lines_iterator(&it) {
        if line == "" {
            ranges_mode = false
        }

        if ranges_mode {
             line_split := strings.split(line, "-")
             start, _ := strconv.parse_int(line_split[0])
             end, _ := strconv.parse_int(line_split[1])
             append(&fresh_ranges, [2]int{start, end})
        } else {
            item, _ := strconv.parse_int(line)
            append(&items, item)
        }
    }
    
    is_fresh := 0
    for item in items {
        fmt.println(item)
        for range in fresh_ranges {
            fmt.println(range)
            if range[0] <= item && item <= range[1] {
                is_fresh += 1
                break
            }
        }
    }


    fmt.println(is_fresh)
}
