package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:slice"
import "core:terminal/ansi"

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


	it := string(data)


    total_sum := 0

    lights := make([dynamic]bit_set[0..=9])
    buttons := make([dynamic][dynamic]bit_set[0..=9])
    joltages := make([dynamic][dynamic]int)

    line_nbr := 0
	for line in strings.split_lines_iterator(&it) {
        row_joltages := make([dynamic]int)
        row_buttons := make([dynamic]bit_set[0..=9])

        splits := strings.split(line, " ")

        for s in splits {
           switch s[0] {
            case '[':
                light: bit_set[0..=9]
                for r, i in s[1:len(s)-1] {
                    if r == '#' {
                        light += {i}
                    }
                }
                append(&lights, light)
            case '(':
                button: bit_set[0..=9]
                for r in s {
                    switch r {
                    case '1':
                        button += {1}
                    case '2':
                        button += {2}
                    case '3':
                        button += {3}
                    case '4':
                        button += {4}
                    case '5':
                        button += {5}
                    case '6':
                        button += {6}
                    case '7':
                        button += {7}
                    case '8':
                        button += {8}
                    case '9':
                        button += {9}
                    case '0':
                        button += {0}
                    }
                }
                append(&row_buttons, button)
            case '{':
                for sub in strings.split(s[1:len(s)-1], ",") {
                    joltage, _ := strconv.parse_int(sub)
                    append(&row_joltages, joltage)
                }
           }
        }

        line_nbr += 1
        append(&buttons, row_buttons)
        append(&joltages, row_joltages)
    }
    delete(data, context.allocator)

    zero_element: bit_set[0..=9]
    to_work := make([dynamic]bit_set[0..=9])
    new_to_work := make([dynamic]bit_set[0..=9])
    shortest_path := make(map[bit_set[0..=9]]int)
    done := make([dynamic]bit_set[0..=9])
    for i in 0..<len(buttons) {
        fmt.println(lights[i], buttons[i], joltages[i])
        clear(&to_work)
        clear(&done)
        append(&to_work, zero_element)
        clear(&shortest_path)
        shortest_path[zero_element] = -1

        found := false
        round := 1
        for !found {
            clear(&new_to_work)
            for len(to_work) > 0 {
                current := pop(&to_work)
                append(&done, current)
                for b in buttons[i] {
                    new_current: bit_set[0..=9] = b ~ current

                    if _, ok := shortest_path[new_current]; !ok && new_current != zero_element {
                        shortest_path[new_current] = round
                    }
                    if new_current == lights[i] {
                        found = true
                        break
                    }
                    
                    already_done := false
                    for d in done {
                        if d == new_current {
                            already_done = true
                            break
                        }
                    }
                    if !already_done {
                        append(&new_to_work, new_current)
                    }


                }
            }
            clear(&to_work)
            for w in new_to_work {
                append(&to_work, w)
            }
            round += 1
        }


        fmt.println(lights[i], "in", shortest_path[lights[i]], "steps.")
        total_sum += shortest_path[lights[i]]
    }
    fmt.println(total_sum)
}
