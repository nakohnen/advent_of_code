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

    letters := make([dynamic][dynamic]rune)
    operations := make([dynamic]rune)
    line_index := 0
	for line in strings.split_lines_iterator(&it) {
        if line_index == 0 {
            for _ in line {
                append(&letters, make([dynamic]rune))
            }
        }
        if line[0] != '*' && line[0] != '+' {
            for r, i in line {
                append(&letters[i], r)
            }
        } else {
            for r, i in line {
                append(&operations, r)
            }

        }

        line_index += 1
    }
    
    lines := make([dynamic]string)
    sb, _ := strings.builder_make_none()
    for line in letters {

        for r in line {
            strings.write_rune(&sb, r)
        }
        s := strings.clone(strings.to_string(sb))
        s = strings.trim(s, " ")
        append(&lines, s)
        strings.builder_reset(&sb)
    }
    strings.builder_destroy(&sb)
    delete(letters)

    for line in lines {
        fmt.println(line)
    }

    numbers_lists := make([dynamic][dynamic]int)
    running_index := 0
    current_list := make([dynamic]int)

    for row in lines {
        if row == "" {
            append(&numbers_lists, current_list)
            fmt.println(current_list)
            current_list = make([dynamic]int)
            running_index += 1
        } else {
            val, _ := strconv.parse_int(row)
            append(&current_list, val)
        }
    }
    append(&numbers_lists, current_list)
    delete(lines)
    fmt.println(numbers_lists)
    fmt.println(operations)
    line_index = 0
    for op in operations {
        if op != ' ' && op != 0 {
        total := 0 
        if op == '*' {
            total = 1
        }
        for nbr in numbers_lists[line_index] {
            switch op {
            case '*': 
                total *= nbr
            case '+':
                total += nbr
            }
        }
        total_sum += total
        line_index += 1
        }

    }

    fmt.println(total_sum)
}
