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
        max_val := 0
        buf: [2]byte
        for i in 0..<len(line)-1 {
            for j in i+1..<len(line) {
                buf[0] = line[i]
                buf[1] = line[j]
                joltage, _ := strconv.parse_int(string(buf[:]))

                if joltage > max_val {
                    max_val = joltage
                }
            }
        }
        fmt.println(max_val)
        total_joltage += max_val
	}
    fmt.println(total_joltage)
}
