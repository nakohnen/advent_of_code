package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:slice"
import "core:terminal/ansi"

get_or_create_id :: proc(ids: ^map[string]int, s: string) -> int {
    if s in ids {
        return ids[s]
    }
    id := len(ids)
    ids[s] = id
    return id
}

count_paths :: proc(connections: [][2]int, current, finish: int) -> int {
    for c in connections {
        fmt.println(current, finish, c)
        start := c[0]
        end := c[1]

        if start == current && end == finish {
            fmt.println("Found end.")
            return 1
        }
    }
    count := 0
    for c in connections {
        start := c[0]
        end := c[1]
        if start == current {
            res := count_paths(connections, end, finish)
            count += res
            fmt.println("res:", res)
        }
    }
    return count
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
	defer delete(data, context.allocator)

    total_sum := 0

    cable_id := make(map[string]int)
    connections := make([dynamic][2]int) // From cable x to cable y
    line_nbr := 0
    it := string(data)
	for line in strings.split_lines_iterator(&it) {
        r_l_split := strings.split(line, ": ")
        defer delete(r_l_split)

        from_c := get_or_create_id(&cable_id, r_l_split[0])

        to_cables_split := strings.split(r_l_split[1], " ")
        defer delete(to_cables_split)

        for to_cable_s in to_cables_split {
            cable_id_r := get_or_create_id(&cable_id, to_cable_s)
            append(&connections, [2]int{from_c, cable_id_r})
        }
        

        line_nbr += 1
    }
    fmt.println("you", cable_id["you"])
    fmt.println("out", cable_id["out"])
    for c in connections {
        fmt.println(c)
    }

    total_sum = count_paths(connections[:], cable_id["you"], cable_id["out"])
    fmt.println(total_sum)
}
