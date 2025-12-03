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

    total_joltage := 0

	for line in strings.split_lines_iterator(&it) {
        buf: [12]byte
        left_i := 0
        len_line := len(line)

        for i in 0..<12 {
            right_i := len(line) - (11 - i)
            max_val := line[left_i]
            max_pos := left_i

            for j in left_i+1..<right_i {
                if line[j] > max_val {
                    max_val = line[j]
                    max_pos = j
                }
            }

            buf[i] = max_val
            left_i = max_pos + 1
        }
        
        joltage, _ := strconv.parse_int(string(buf[:]))

        fmt.println(joltage)
        total_joltage += joltage
	}
    fmt.println(total_joltage)
}
