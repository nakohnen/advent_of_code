package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:slice"

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
	for line in strings.split_lines_iterator(&it) {
        if line == "" {
            break
        }

        line_split := strings.split(line, "-")
        start, _ := strconv.parse_int(line_split[0])
        end, _ := strconv.parse_int(line_split[1])
        append(&fresh_ranges, [2]int{start, end})
    }

    new_fresh_ranges := make([dynamic][2]int)

    cmp_2i_arr :: proc(a1, a2: [2]int) -> bool {
        if a1[0] < a2[0] {
            return true
        } else if a1[0] == a2[0] {
            if a1[1] <= a2[1] {
                return true
            }
        }
        return false
    }

    slice.sort_by(fresh_ranges[:], cmp_2i_arr) 
    fmt.println(fresh_ranges)
    for range, i in fresh_ranges {
        start := range[0]
        end := range[1]

        already_done := false
        for range2 in new_fresh_ranges {
            if range2[0] <= start && end <= range2[1] {
                already_done = true
                break
            }
        }
        if already_done {
            continue
        }

        for range2 in fresh_ranges[i:] {
            start2 := range2[0]
            end2 := range2[1]

            if start <= end2 && start2 <= end {
                start = min(start, start2)
                end = max(end, end2)
            }
        }
        fmt.println(range)
        fmt.println(new_fresh_ranges)

        append(&new_fresh_ranges, [2]int{start, end})
    }
    
    delete(fresh_ranges)

    is_fresh := 0
    for range in new_fresh_ranges {
        fmt.println(range)
        is_fresh += range[1] - range[0] + 1
    }

    fmt.println(new_fresh_ranges)
    fmt.println(is_fresh)
}
