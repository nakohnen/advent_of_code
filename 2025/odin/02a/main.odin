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

    invalid_sum := 0

	for line in strings.split_lines_iterator(&it) {
        for sub_line in strings.split(line, ",") {
            split_again := strings.split(sub_line, "-")
            left_raw := split_again[0]
            right_raw := split_again[1]

            left, _ :=  strconv.parse_int(left_raw)
            right, _ := strconv.parse_int(right_raw)

            for val in left..=right {
                val_str := fmt.aprintf("%d", val)
                if len(val_str) % 2 == 0 {
                    if val_str[:len(val_str)/2] == val_str[len(val_str)/2:] {
                        invalid_sum += val
                    }
                }
                delete(val_str)
            }
        }
	}
    fmt.println(invalid_sum)
}
