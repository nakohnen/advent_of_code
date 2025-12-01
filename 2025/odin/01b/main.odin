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

    position := 50
    zero_count := 0
    rotation := 0

	it := string(data)
	for line in strings.split_lines_iterator(&it) {

        rotation, _ = strconv.parse_int(line[1:])
        for _ in 0..<rotation {
            rot := 1
            if line[0] == 'L'{
                rot = -1
            }
            position += rot
            position %= 100

            if position == 0 {
                zero_count += 1
            }

        }

	}
    fmt.println(zero_count)
}
