package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:slice"
import "core:terminal/ansi"
import "core:thread"
import "core:mem"

VERBOSE_OUTPUT :: false

get_or_create_id :: proc(ids: ^map[string]int, s: string) -> int {
    if s in ids {
        return ids[s]
    }
    id := len(ids)
    ids[s] = id
    return id
}

is_in :: proc(l: []$T, e: T) -> bool {
    for i in l {
        if i == e {
            return true
        }
    }
    return false
}

follow_to_end :: proc(connections: [][2]int, current: int, exceptions: []int) -> int {
    when VERBOSE_OUTPUT {
        fmt.println("Following", current)
    }
    
    for c in connections {
        start := c[0]
        end := c[1]

        if start == current {
            if !is_in(exceptions, end) {
                when VERBOSE_OUTPUT {
                    fmt.println("End:", current, end)
                }
                return end
            }

            for c2 in connections {
                start2 := c2[0]
                end2 := c2[1]
                if end == start2 {
                    when VERBOSE_OUTPUT {
                        fmt.println("Following", current, start2)
                    }
                    return follow_to_end(connections, start2, exceptions)
                }
            }
            when VERBOSE_OUTPUT {
                fmt.println("End:", current, end)
            }
            return end
        }
    }
    when VERBOSE_OUTPUT {
        fmt.println("End:", current, -1)
    }
    return -1
}

reduce_network_size :: proc(connections: [][2]int, exceptions: []int) -> [dynamic][2]int {
    max_id := 0
    for c in connections {
        max_id = max(c[0], max_id)
        max_id = max(c[1], max_id)
    }

    incoming := make([dynamic]int, max_id+1)
    outgoing := make([dynamic]int, max_id+1)
    defer delete(incoming)
    defer delete(outgoing)

    for c in connections {
        start := c[0]
        end := c[1]
        incoming[end] += 1
        outgoing[start] += 1
    }

    for e in exceptions {
        incoming[e] = max(int)
        outgoing[e] = max(int)
    }

    to_reduce := make([dynamic]int)
    defer delete(to_reduce)

    for i in 0..<len(incoming) {
        if incoming[i] == 1 && outgoing[i] == 1 {
            append(&to_reduce, i)
        }
    }
    when VERBOSE_OUTPUT {
        fmt.println("To reduce:", to_reduce)
    }

    replace_connections := make([dynamic][2]int)
    edges_connections := make([dynamic][2]int)
    right_edges_connections := make([dynamic][2]int)
    remaining_connections := make([dynamic][2]int)
    defer delete(replace_connections)
    defer delete(edges_connections)
    defer delete(right_edges_connections)
    for c in connections {
        start := c[0]
        end := c[1]
        start_is_in := is_in(to_reduce[:], start)
        end_is_in := is_in(to_reduce[:], end)
        if start_is_in || end_is_in {
            append(&replace_connections, c)
        } else {
            append(&remaining_connections, c)
        }

        if !start_is_in && end_is_in {
            append(&edges_connections, c)
        } else if start_is_in && !end_is_in {
            append(&right_edges_connections, c)
        }
    }

    when VERBOSE_OUTPUT {
        for r in replace_connections {
            fmt.println("To replace:", r)
        }
    }

    for edge in edges_connections {
        when VERBOSE_OUTPUT {
            fmt.println("Edge:", edge)
        }

        start := edge[0]
        end := edge[1]
        new_end := follow_to_end(replace_connections[:], end, to_reduce[:])
        new_c := [2]int{start, new_end}
        when VERBOSE_OUTPUT {
            fmt.println("New connection:", new_c, "from", edge)
        }
        if new_end >= 0 {
            append(&remaining_connections, new_c)
        }
    }

    return remaining_connections
}

propagate :: proc(connections: [][2]int, current: int, source_index: int = 0, target_index: int = 1) -> [dynamic]int {
    touched_by := make([dynamic]int)
    append(&touched_by, current)

    last_len := 0

    work_connections := make([dynamic][2]int, len(connections))
    for c, i in connections {
        work_connections[i] = c
    }
    defer delete(work_connections)

    new_work_connections := make([dynamic][2]int)
    defer delete(new_work_connections)

    for last_len != len(touched_by) {
        last_len = len(touched_by)

        for len(work_connections) > 0 {
            con := pop(&work_connections)
            source := con[source_index]
            target := con[target_index]
            if is_in(touched_by[:], source) && !is_in(touched_by[:], target) {
                append(&touched_by, target)
            } else {
                append(&new_work_connections, con)
            }
        }

        work_connections, new_work_connections = new_work_connections, work_connections
        clear(&new_work_connections)
    }
    return touched_by
}

count_paths_simple :: proc(connections: [][2]int, current, finish: int, visited: []int) -> int {
    count := 0
    for c in connections {
        //fmt.println(current, finish, c)
        start := c[0]
        end := c[1]

        if start == current && end == finish {
            //fmt.println("Found end.")
            count += 1
        }
    }
    if count > 0 {
        return count
    }
    for c in connections {
        start := c[0]
        end := c[1]

        
        if start == current {
            when VERBOSE_OUTPUT {
                fmt.println("Path", c, "count=", count)
            }
            if !is_in(visited[:], end) {
                new_visited := make([dynamic]int)
                defer delete(new_visited)
                for v in visited {
                    append(&new_visited, v)
                }
                append(&new_visited, end)

                res := count_paths_simple(connections, end, finish, new_visited[:])
                count += res
                // fmt.println("res:", res)
            }
        }
    }
    return count
}

count_paths :: proc(connections: [][2]int, current, finish: int, visited: []int, dac, fft:int) -> int {

    for c in connections {
        //fmt.println(current, finish, c)
        start := c[0]
        end := c[1]

        if start == current && end == finish {
            new_visited := make([dynamic]int)
            defer delete(new_visited)
            for v in visited {
                append(&new_visited, v)
            }
            append(&new_visited, end)

            found_dac := false
            found_fft := false
            for v in new_visited {
                if v == dac {
                    found_dac = true
                }
                if v == fft {
                    found_fft = true
                }
                if found_dac && found_fft {
                    return 1
                }
            }
            return 0
        }
    }

    count := 0
    for c in connections {
        start := c[0]
        end := c[1]
        if start == current && !is_in(visited[:], end) {
            new_visited := make([dynamic]int)
            defer delete(new_visited)
            for v in visited {
                append(&new_visited, v)
            }
            append(&new_visited, end)

            when VERBOSE_OUTPUT {
                if end == dac {
                    fmt.println("Passing dac", new_visited)
                }
                if  end == fft {
                    fmt.println("Passing fft", new_visited)
                }
            }
            res := count_paths(connections, end, finish, new_visited[:], dac, fft)
            count += res
            //fmt.println("res:", res)
        }
    }
    return count
}

count_paths_wrapper :: proc(connections: [][2]int, current, finish: int) -> int {
    new_visited := make([dynamic]int)
    defer delete(new_visited)
    append(&new_visited, current)
    return count_paths_simple(connections, current, finish, new_visited[:])
}

WorkData :: struct {
    start_node: int,
    end_node: int,
    connections: [][2]int,
}

Task_Data :: struct {
    item: ^WorkData,
    result: ^int,
}

process_one :: proc(item: WorkData) -> int {
    fmt.printf("Started: {0} -> {1}\n", item.start_node, item.end_node)
    visited := make([dynamic]int)
    defer delete(visited)
    res := count_paths_simple(item.connections[:], item.start_node, item.end_node, visited[:])
    fmt.printf("Done: {0} -> {1} in {2} possibilities.\n", item.start_node, item.end_node, res)
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
    // Redundant since the cable_id has the node list, its a simple integer...
    // nodes := make([dynamic]int)
    // for c in connections {
    //     for node in c {
    //         if is_in(nodes[:], node) {
    //             append(&nodes, node)
    //         }
    //     }
    // }
    

    fmt.println("svr", cable_id["svr"])
    fmt.println("out", cable_id["out"])
    fmt.println("fft", cable_id["fft"])
    fmt.println("dac", cable_id["dac"])
    // for c in connections {
    //     fmt.println(c)
    // }

    exceptions := make([dynamic]int)
    append(&exceptions, cable_id["svr"])
    append(&exceptions, cable_id["out"])
    append(&exceptions, cable_id["dac"])
    append(&exceptions, cable_id["fft"])
    fmt.println("Exceptions:", exceptions)

    when VERBOSE_OUTPUT {
        for new_c in connections {
            fmt.printf("{0} -> {1}\n", new_c[0], new_c[1])
            //fmt.println(new_c)
        }
    } else {
        fmt.printf("Connections count: {0}\n", len(connections))
    }

    forw_prop_dac := propagate(connections[:], cable_id["dac"], 0, 1)
    back_prop_dac := propagate(connections[:], cable_id["dac"], 1, 0)
    forw_prop_fft := propagate(connections[:], cable_id["fft"], 0, 1)
    back_prop_fft := propagate(connections[:], cable_id["fft"], 1, 0)

    when VERBOSE_OUTPUT {
        for n in forw_prop_dac {
            fmt.println("Forward propagate dac:", n)
        }
        for n in back_prop_dac {
            fmt.println("Back propagate dac:", n)
        }
        for n in forw_prop_fft {
            fmt.println("Forward propagate fft:", n)
        }
        for n in back_prop_fft {
            fmt.println("Back propagate fft:", n)
        }

    }
    
    dac_nodes := make([dynamic]int)
    fft_nodes := make([dynamic]int)
    for node in back_prop_dac {
        append(&dac_nodes, node)
    }
    for node in forw_prop_dac {
        append(&dac_nodes, node)
    }
    for node in back_prop_fft {
        append(&fft_nodes, node)
    }
    for node in forw_prop_fft {
        append(&fft_nodes, node)
    }
    defer delete(back_prop_dac)
    defer delete(forw_prop_dac)
    defer delete(back_prop_fft)
    defer delete(forw_prop_fft)
    defer delete(dac_nodes)
    defer delete(fft_nodes)

    reduced_nodes := make([dynamic]bool, len(cable_id))
    for i in 0..<len(cable_id) {
        reduced_nodes[i] = is_in(dac_nodes[:], i) && is_in(fft_nodes[:], i) 
    }

    reduced_connections := make([dynamic][2]int)
    for c in connections {
        if reduced_nodes[c[0]] && reduced_nodes[c[1]] {
            append(&reduced_connections, c)
        }
    }

    when VERBOSE_OUTPUT {
        fmt.println("Reduced connections:")
        for new_c in reduced_connections {
            fmt.printf("{0} -> {1}\n", new_c[0], new_c[1])
            //fmt.println(new_c)
        }
    } else {
        fmt.printf("Reduced connections to {0}\n", len(reduced_connections))
    }

    // Who comes first?
    start_node := cable_id["svr"]
    first := -1
    second := -1
    end_node := cable_id["out"]

    if is_in(forw_prop_dac[:], cable_id["fft"]) && is_in(back_prop_fft[:], cable_id["dac"]) {
        first = cable_id["dac"]
        second = cable_id["fft"]
        fmt.println("dac comes before fft")
    } else if is_in(forw_prop_fft[:], cable_id["dac"]) && is_in(back_prop_dac[:], cable_id["fft"]) {
        first = cable_id["fft"]
        second = cable_id["dac"]
        fmt.println("fft comes before dac")
    } else {
        fmt.println("Something weird going on")
        return
    }
    fmt.printf("{0} -> {1} -> {2} -> {3}\n", start_node, first, second, end_node)

    work_data := make([dynamic]WorkData, 3)
    work_data[0] = WorkData{start_node, first, reduced_connections[:]}
    work_data[1] = WorkData{first, second, reduced_connections[:]}
    work_data[2] = WorkData{second, end_node, reduced_connections[:]}

    result := parallel_map(work_data[:])

    total_sum = 1
    for r in result {
        total_sum *= r
    }

    fmt.println(total_sum)

    return
}
