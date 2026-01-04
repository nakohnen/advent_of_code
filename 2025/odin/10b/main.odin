package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:slice"
import "core:terminal/ansi"
import vmem "core:mem/virtual"
import "core:thread"
import "core:mem"
import "core:sys/info"

VERBOSE_OUTPUT :: false

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

write_map_to_disk :: proc(filename: string, data: map[[10]int]int) -> bool {
    b := strings.builder_make()
    defer strings.builder_destroy(&b)
    for key in data {
        fmt.sbprintln(&b, key, data[key])
    }
    output_txt := strings.to_string(b)

    // os.write_entire_file(filepath, output_txt[:])
    return os.write_entire_file(filename, transmute([]u8)output_txt)
}

sort_by_card :: proc(v1, v2: [10]int) -> bool {
    sum1 := 0
    sum2 := 0
    for x in v1 {
        sum1 += x
    }
    for x in v2 {
        sum2 += x
    }
    return sum1 > sum2
}

WorkData :: struct {
    target: [10]int,
    vectors: [][10]int,
}

Task_Data :: struct {
    item: ^WorkData,
    result: ^int,
}

process_one :: proc(item: WorkData) -> int {
    fmt.println("Started:", item.target, item.vectors)
    res := solve2(item.target, item.vectors[:])
    fmt.println("Done:", item.target, res)
    return res
}

worker :: proc(task: thread.Task) {
    data := cast(^Task_Data)task.data
    data.result^ = process_one(data.item^)
}

parallel_map :: proc(input: []WorkData, allocator := context.allocator) -> [dynamic]int {
    count := len(input)
    
    results := make([dynamic]int, count, allocator)
    mem.zero_slice(results[:])
    
    if count == 0 do return results
    
    pool: thread.Pool
    thread.pool_init(&pool, allocator, thread_count = 12)
    defer thread.pool_destroy(&pool)
    thread.pool_start(&pool)
    
    // Task data array - lives until pool_finish returns
    tasks := make([]Task_Data, count, context.temp_allocator)
    
    for i in 0..<count {
        tasks[i] = Task_Data{
            item   = &input[i],
            result = &results[i],
        }
        thread.pool_add_task(&pool, allocator, worker, &tasks[i])
    }
    
    thread.pool_finish(&pool)
    
    return results
}

solve2 :: proc(target: [10]int, vectors: [][10]int, rec: int = 0) -> int {
    when VERBOSE_OUTPUT {
        if rec >= 0 {
            fmt.println("Solving:", target, vectors)
        }
    }
    // Solve recursively
    // Search for the place with the fewest possible matching vectors
    // For all matching vectors do a combination matching up the the target
    // i.e. if target[i] = 5 and we have 2 vectors then we take 
    // 5-0; 4-1; 3-2; 2-3; 1-4; 0-5.
    // For each combination substract that from the target, remove both vectors
    // And solve again lower
    // For each combination we then take the one with the smallest steps if the steps are > 0 
    // else it was invalid

    max_t := -1
    for t in target {
        max_t = max(max_t, t)
        if t < 0 {
            when VERBOSE_OUTPUT {
                fmt.println("t < 0", target, t)
            }
            return -1
        }
    }

    all_zero := true
    for t in target {
        if t != 0 {
            all_zero = false
            break
        }
    }
    if all_zero {
        when VERBOSE_OUTPUT {
            fmt.println("All zeros, returning 0")
        }
        return 0
    }

    if len(vectors) == 0 {
        return -1
    } else if len(vectors) == 1 {
        v := vectors[0]

        min_v, min_index := get_smallest(target, 0)

        new_target := target - min_v * v

        for t in new_target {
            if t != 0 {
                when VERBOSE_OUTPUT {
                    fmt.println("t != 0", new_target, t)
                }
                return -1
            }
        }
        return min_v

    }

    vectors_2 := make([dynamic][10]int)
    defer delete(vectors_2)
    for v in vectors {
        out := false
        for t_v, t_i in v {
            if target[t_i] == 0 && t_v > 0 {
                out = true
                break
            }
        }

        if !out {
            append(&vectors_2, v)
        }
    }
    when VERBOSE_OUTPUT {
        if rec >= 0 && len(vectors) != len(vectors_2) {
            fmt.println("Reduced the vectors to:", vectors_2)
        }

    }


    results := make([dynamic]int)
    defer delete(results)

    indexes_targeted: [10]int
    for b in vectors_2 {
        indexes_targeted += b
    }

    min_v, min_index := get_smallest(target, 0)
    t_val := min_v
    t_index := min_index

    if min_index == -1 {
        return 0
    }

    sub_vectors := make([dynamic][10]int)
    other_vectors := make([dynamic][10]int)
    defer delete(sub_vectors)
    defer delete(other_vectors)

    for b in vectors_2 {
        if b[t_index] == 1 {
            append(&sub_vectors, b) }
        else {
            append(&other_vectors, b)
        }
    }

    if len(sub_vectors) == 0 {
        return -1
    }
    
    slice.sort_by(sub_vectors[:], sort_by_card)
    slice.sort_by(other_vectors[:], sort_by_card)

    comb := make([dynamic]int, len(sub_vectors))
    defer delete(comb)
    comb[0] = t_val

    for {
        new_target := target
        for n, i in comb {
            new_target -= n * sub_vectors[i]
        }
        when VERBOSE_OUTPUT {
            if rec >= 0 {
                fmt.println("\t", comb, new_target, sub_vectors)
            }
        }


        // Sanity check
        valid_target := true
        for n in new_target {
            if n < 0 {
                when VERBOSE_OUTPUT {
                    fmt.println("Panic: Sanity check not passed:", new_target)
                }
                valid_target = false
                break
            }
        }
        if valid_target {
            if sub_res := solve2(new_target, other_vectors[:], rec + 1); sub_res >= 0 && sub_res + t_val >= max_t {

                when VERBOSE_OUTPUT {
                    if rec >= 0 {
                        fmt.println("\t Result added", t_val+sub_res, t_val, sub_res, comb)
                    }
                }
                append(&results, t_val + sub_res)
            }
        }

        // --- Exit condition: all weight in last bin ---
        if comb[len(comb) - 1] == t_val {
            break
        }

        // --- Find rightmost non-zero before last ---
        i := len(comb) - 2
        for i >= 0 && comb[i] == 0 {
            i -= 1
        }

        // --- Collect tail ---
        tail := 0
        for j in (i + 1)..<len(comb) {
            tail += comb[j]
        }

        // --- Advance: decrement i, put tail+1 in i+1, zero rest ---
        comb[i] -= 1
        comb[i + 1] = tail + 1
        for j in (i + 2)..<len(comb) {
            comb[j] = 0
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
    
    
    when VERBOSE_OUTPUT {
        if rec >= 0 {
            fmt.println("Result:", min_r, "from", results)
        }
    }
    return min_r
}

main :: proc() {
    if len(os.args) - 1 != 1 {
        fmt.println("Need exactly one argument with the filename")
        return
    }
    filepath := os.args[1]


    total_sum := 0
    lights := make([dynamic][10]int)
    buttons := make([dynamic][dynamic][10]int)
    joltages := make([dynamic][10]int)

    {
        data, ok := os.read_entire_file(filepath, context.allocator)
        if !ok {
            fmt.println("Could not read file.")
            return
        }
        it := string(data)
        line_nbr := 0
        for line in strings.split_lines_iterator(&it) {
            row_buttons := make([dynamic][10]int)

            splits := strings.split(line, " ")
            defer delete(splits)

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
                    joltage_splits := strings.split(s[1:len(s)-1], ",")
                    for sub in joltage_splits {
                        joltage, _ := strconv.parse_int(sub)
                        joltage_total[index] = joltage
                        index += 1
                    }
                    append(&joltages, joltage_total)
                    delete(joltage_splits)
                }
            }

            line_nbr += 1
            append(&buttons, row_buttons)
        }
        delete(data, context.allocator)
    }

    result_already_done := make(map[[10]int]int)
    {
        data, ok := os.read_entire_file("working.txt", context.allocator)
        if ok {
            it := string(data)
            line_nbr := 0
            for line in strings.split_lines_iterator(&it) {
                splits := strings.split(line, "]")
                defer delete(splits)
                s := splits[0]
                joltage_total: [10]int
                index := 0
                sub_splits := strings.split(s[1:], ",") 
                defer delete(sub_splits)
                for sub in sub_splits {
                    joltage, _ := strconv.parse_int(strings.trim_space(sub))
                    joltage_total[index] = joltage
                    index += 1
                }
                joltage_result, _ := strconv.parse_int(strings.trim_space(splits[1]))
                result_already_done[joltage_total] = joltage_result
            }
        }
        delete(data, context.allocator)
        
    }

    work_data := make([dynamic]WorkData)
    for i in 0..<len(joltages) {
        if res, ok := result_already_done[joltages[i]]; ok {
            total_sum += res
        } else {
            append(&work_data, WorkData{joltages[i], buttons[i][:]})
            when VERBOSE_OUTPUT {
                fmt.println(joltages[i])
                for b in buttons[i] {
                    fmt.println("Button:", b)
                }
            }
        }
    }

    delete(joltages)
    delete(lights)

    result := parallel_map(work_data[:])

    for res in result {
        total_sum += res
    }
    fmt.println(total_sum)
}
