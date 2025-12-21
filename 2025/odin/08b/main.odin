package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:slice"

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
	defer delete(data, context.allocator)


	it := string(data)

    j_box := make([dynamic][3]int)

    total_sum := 0

    line_nbr := 0
	for line in strings.split_lines_iterator(&it) {
        splits := strings.split(line, ",")
        new_j := [3]int{0,0,0}
        for s, i in splits {
            nbr, _ := strconv.parse_int(s)
            new_j[i] = nbr
        }
        append(&j_box, new_j)

        line_nbr += 1
    }

    distances := make([dynamic]Connection)

    for b1, index in j_box[:len(j_box)-1] {
        for b2, index2 in j_box {
            if index2 < index+1 {
                continue
            }
            // fmt.print(index, index2, " ")
            // fmt.print(b1, b2, " ")

            distance :f64 = 0.0
            inner := math.pow(f64(b1.x - b2.x), 2.0) + math.pow(f64(b1.y - b2.y), 2.0) + math.pow(f64(b1.z - b2.z), 2.0)
            distance = math.sqrt(inner)
            // fmt.println(distance)
            append(&distances, Connection{distance, [2]int{index, index2}})
        }
    }

    cmp_connctions :: proc(c1, c2: Connection) -> bool {
        return c1.distance <= c2.distance
    }
    slice.sort_by(distances[:], cmp_connctions)

    box_group := make([dynamic]int)

    for _, i in j_box {
        append(&box_group, i)
    }

    current_group := 0

    target_distance: Connection
    
    for c in distances {
        b1_index := c.boxes[0]
        b2_index := c.boxes[1]

        if box_group[b1_index] != box_group[b2_index]{
            fmt.print(j_box[b1_index], j_box[b2_index], box_group[b1_index], box_group[b2_index])
            current_max := 0
            // Merge groups into smallest int of both indexes
            new_group := min(box_group[b1_index], box_group[b2_index])
            to_overwrite := max(box_group[b1_index], box_group[b2_index])

            for val, key in box_group {
                if box_group[key] == to_overwrite {
                    box_group[key] = new_group
                }

                current_max = max(box_group[key], current_max)
            }

            if box_group[b1_index] != box_group[b2_index]{
                fmt.println("We got an error here:", c)
                break
            }
            fmt.println(" ->", box_group[b1_index], box_group[b2_index], "max=", current_max)
            if current_max == 0 {
                target_distance = c
                break
            }
        }
    }

    total_sum = j_box[target_distance.boxes[0]].x * j_box[target_distance.boxes[1]].x

    fmt.println(total_sum)
}
