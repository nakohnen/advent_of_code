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


    total_sum := 0

    numbers := make([dynamic][dynamic]int)
    line_index := 0
	for line in strings.split_lines_iterator(&it) {
        trimmed_line := strings.trim(line, " ")
        split_lines_tmp := strings.split(trimmed_line, " ", context.temp_allocator)
        split_lines := make([dynamic]string, context.temp_allocator)
        for s in split_lines_tmp {
            if s != "" {
                append(&split_lines, s)
            }
        }
        //fmt.println(split_lines)

        for nbr_str, i in split_lines {
            if nbr_str != "*" && nbr_str != "+" {
                nbr, _ := strconv.parse_int(nbr_str)
                if line_index == 0 {
                    sub_list := make([dynamic]int)
                    append(&numbers, sub_list)
                }
                append(&numbers[i], nbr)
            } else {
                total: int
                switch nbr_str {
                case "+":
                    total = 0
                case "*":
                    total = 1
                }
                fmt.println(nbr_str, numbers[i])
                for nbr in numbers[i] {
                    switch nbr_str {
                        case "+":
                            total += nbr
                        case "*":
                            total *= nbr
                    }
                }
                total_sum += total
            }
        }
        line_index += 1
    }


    fmt.println(total_sum)
}
