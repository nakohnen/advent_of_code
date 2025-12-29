package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:slice"
import "core:terminal/ansi"


get_smallest :: proc(input: [10]int, lower_bound: int) -> (int, int) {
    min:= max(int)
    index := -1
    for v, i in input {
        if v < min && v > lower_bound {
            min = v
            index = i
        }
    }
    return min, index
}

sum_arr :: proc(input: []int) -> int {
    r := 0
    for v in input {
        r += v
    }
    return r
}

gt_0 :: proc(input: []int) -> bool {
    r := len(input)
    for v in input {
        if v <= 0 {
            r -= 1
        }
    }
    return r > 0
}


get_combinations :: proc(target_val, elements: int) -> [dynamic][dynamic]int {
    working_set := make([dynamic][dynamic]int)
    for index in 0..<elements {
        if index == 0 {
            for x in 0..=target_val {
                comb := make([dynamic]int)
                append(&comb, x)
                append(&working_set, comb)
            }
        } else {
            new_working_set := make([dynamic][dynamic]int)
            
            is_last := index == elements - 1

            for len(working_set) > 0 {
                current := pop(&working_set)
                remaining := target_val - sum_arr(current[:])

                // Sanity check
                if remaining < 0 {
                    delete(current)
                    continue
                }

                if is_last {
                    new_comb := make([dynamic]int)
                    for v in current {
                        append(&new_comb, v)
                    }
                    append(&new_comb, remaining)
                    append(&new_working_set, new_comb)
                } else {
                    for x in 0..=max(remaining, 0) {
                        new_comb := make([dynamic]int)
                        for v in current {
                            append(&new_comb, v)
                        }
                        append(&new_comb, x)
                        append(&new_working_set, new_comb)
                    }
                }

                delete(current)
            }
            
            delete(working_set)
            working_set = new_working_set
        }
    }
    return working_set
}

solve :: proc(target: [10]int, vectors: [][10]int) -> int {
    // Solve recursively
    // Search for the place with the fewest possible matching vectors
    // For all matching vectors do a combination matching up the the target
    // i.e. if target[i] = 5 and we have 2 vectors then we take 
    // 5-0; 4-1; 3-2; 2-3; 1-4; 0-5.
    // For each combination substract that from the target, remove both vectors
    // And solve again lower
    // For each combination we then take the one with the smallest steps if the steps are > 0 
    // else it was invalid
    if len(vectors) == 0 {
        return 0
    } else if len(vectors) == 1 {
        v := vectors[0]

        min_v, min_index := get_smallest(target, 0)

        new_target := target - min_v * v

        for t in new_target {
            if t != 0 {
                return -1
            }
        }
        return min_v

    }


    min_v, min_index := get_smallest(target, 0)
    //fmt.println("min", min, "min_index", min_index)
    //fmt.println(target, vectors, min_v, min_index)

    // Create a list of all buttons targeting the index
    sub_vectors := make([dynamic][10]int)
    defer delete(sub_vectors)
    other_vectors := make([dynamic][10]int)
    defer delete(other_vectors)
    for b in vectors {
        if b[min_index] == 1 {
            append(&sub_vectors, b) }
        else {
            append(&other_vectors, b)
        }
    }

    if len(sub_vectors) == 0 {
        return -1
    }
    
    combs := get_combinations(min_v, len(sub_vectors))
    defer delete(combs)

    result := min_v
    sub_result := 0
    results := make([dynamic]int)
    defer delete(results)
    for comb in combs {
        defer delete(comb)
        new_target := target
        for n, i in comb {
            new_target -= n * sub_vectors[i]
        }
        target_ok := true
        for t in new_target {
            if t < 0 {
                target_ok = false
                break
            }
        }
        if !target_ok {
            continue
        }
        

        sub_res := solve(new_target, other_vectors[:])
        if sub_res >= 0 {
            append(&results, sub_res)
        }
    }

    sol_found := false
    min_r := max(int)
    for r in results {
        if r >= 0 {
            sol_found = true
            min_r = min(min_r, r)
        }
    }

    if !sol_found {
        return -1
    }


    return result + min_r
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

    lights := make([dynamic][10]int)
    buttons := make([dynamic][dynamic][10]int)
    joltages := make([dynamic][10]int)

    line_nbr := 0
	for line in strings.split_lines_iterator(&it) {
        row_buttons := make([dynamic][10]int)

        splits := strings.split(line, " ")

        for s in splits {
           switch s[0] {
            case '[':
                light: [10]int
                for r, i in s[1:len(s)-1] {
                    if r == '#' {
                        light[i] += 1
                    }
                }
                append(&lights, light)
            case '(':
                button: [10]int
                for r in s {
                    switch r {
                    case '1':
                        button[1] += 1
                    case '2':
                        button[2] += 1
                    case '3':
                        button[3] += 1
                    case '4':
                        button[4] += 1
                    case '5':
                        button[5] += 1
                    case '6':
                        button[6] += 1
                    case '7':
                        button[7] += 1
                    case '8':
                        button[8] += 1
                    case '9':
                        button[9] += 1
                    case '0':
                        button[0] += 1
                    }
                }
                append(&row_buttons, button)
            case '{':
                joltage_total: [10]int
                index := 0
                for sub in strings.split(s[1:len(s)-1], ",") {
                    joltage, _ := strconv.parse_int(sub)
                    joltage_total[index] = joltage
                    index += 1
                }
                append(&joltages, joltage_total)
           }
        }

        line_nbr += 1
        append(&buttons, row_buttons)
    }
    delete(data, context.allocator)


    for i in 0..<len(buttons) {
        fmt.println(joltages[i]) 
        fmt.println("Buttons")
        for b in buttons[i] {
            fmt.println(b)
        }

        shortest_path := max(int)


        current_joltage: [10]int
        current_joltage += joltages[i]

        steps := 0

        // Get the quick easy wins:
        indexes_targeted: [10]int
        for b in buttons[i] {
            indexes_targeted += b
        }
        fmt.println("indexes_targeted", indexes_targeted)
        new_buttons := make([dynamic][10]int)
        for b in buttons[i] {
            used := false
            for t, j in indexes_targeted {
                if t == 1 {
                    if b[j] == t {
                        used = true
                        steps += current_joltage[j]
                        current_joltage -= current_joltage[j] * b
                    }
                }
            }
            if !used {
                append(&new_buttons, b)
            }
        }


        fmt.println("current joltage:", current_joltage)

        steps += solve(current_joltage, new_buttons[:])

        fmt.println(joltages[i], "in", steps, "steps.")
        total_sum += steps
    }
    fmt.println(total_sum)
}
