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
    beam_splitters := make([dynamic][dynamic]bool)
    beams := make([dynamic][dynamic]int)

    line_nbr := 0
	for line in strings.split_lines_iterator(&it) {
        new_beam_splitters := make([dynamic]bool)
        new_beams := make([dynamic]int)

        for r, index in line {
            append(&new_beams, 0)
            switch r {
            case '.':
                append(&new_beam_splitters, false)
            case 'S':
                append(&new_beam_splitters, false)
                new_beams[index] = 1
            case '^':
                append(&new_beam_splitters, true)
            }
        }
        if line_nbr > 0 {
            for s, index in new_beam_splitters {
                if new_beam_splitters[index] && beams[line_nbr-1][index] > 0 {
                    nbr_add := beams[line_nbr-1][index]
                    new_beams[index] = 0
                    new_beams[index - 1] += nbr_add
                    new_beams[index + 1] += nbr_add
                } else {
                    new_beams[index] = beams[line_nbr-1][index] + new_beams[index]
                }

            }
        }
        append(&beam_splitters, new_beam_splitters)
        append(&beams, new_beams)

        for i in 0..<len(new_beams) {
            if new_beams[i]>0 {
                fmt.print("|")
            } else if new_beam_splitters[i] {
                fmt.print("^")
            } else {
                fmt.print(".")
            }
        }
        fmt.println("")
        line_nbr += 1
    }

    for nbr in beams[len(beams)-1] {
        total_sum += nbr
    }

    fmt.println(total_sum)
}
