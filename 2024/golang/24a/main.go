package main

import (
	"bufio"
	"fmt"
	"os"
    //"sync"
    "strings"
    "strconv"
)

func readInt(s string) int {
	val, err := strconv.Atoi(s)
	if err != nil {
		//fmt.Printf("Error converting str %s to int\n", s)
		os.Exit(1)
	}
	return val
}

func readBin(s string) int {
    res, err := strconv.ParseInt(s, 2, 64)
	if err != nil {
		fmt.Println("Error:", err)
	}
    return int(res)
}

func apply(a, b int, op string) int {
    switch op {
    case "XOR":
        return a ^ b
    case "OR":
        return a | b
    case "AND":
        return a & b
    }
    os.Exit(1)
    return -1
}

type gate struct {
    regA, regB, op string
}


// RemoveElement removes the first occurrence of an element from a slice
func RemoveElement[T comparable](slice []T, element T) []T {
	for i, v := range slice {
		if v == element {
			// Remove the element and return the new slice
			return append(slice[:i], slice[i+1:]...)
		}
	}
	// If the element is not found, return the original slice
	return slice
}


func resolve(states map[string]int, gates map[string]gate) int {
    toCalculate := []string{}
    for reg, val := range states {
        if val == -1 {
            toCalculate = append(toCalculate, reg)
        }
    }

    maxZ := 0
    zStates := make(map[int]int)
    
    for len(toCalculate) > 0 {
        current := toCalculate[0]
        toCalculate = toCalculate[1:]

        g := gates[current]
        valA := states[g.regA]
        valB := states[g.regB]
        
        zReg := 0
        if current[0] == 'z' {
            zReg = readInt(current[1:])
            if zReg > maxZ {
                maxZ = zReg
            }
        }

        if valA != -1 && valB != -1 {
            val := apply(valA, valB, g.op)
            states[current] = val
            if current[0] == 'z' {
                zStates[zReg] = val
            }
            fmt.Printf("%v = %v %v %v = %d %v %d = %d\n", current, g.regA, g.op, g.regB, valA, g.op, valB, val)
        } else {
            toCalculate = append(toCalculate, current)
        }
    }
    bin := ""
    if maxZ > 0 {
        for z:=0;z<=maxZ;z++{
            bin = fmt.Sprintf("%d", zStates[z]) + bin
        }
        return readBin(bin)
    }
    return 0
}

func main() {
	// Check if enough arguments are provided
	if len(os.Args) != 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		os.Exit(1)
	}

	// Parse command-line arguments
	inputFile := os.Args[1]

	// Open the input file
	inFile, err := os.Open(inputFile)
	if err != nil {
		fmt.Printf("Error opening input file: %v\n", err)
		os.Exit(1)
	}
	defer inFile.Close()

	// Prepare for line-by-line reading and writing
	scanner := bufio.NewScanner(inFile)

	sum := 0

    states := make(map[string]int)
    gates := make(map[string]gate)

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

        if strings.Contains(line, ":") {
            splits := strings.Split(line, ": ")
            states[splits[0]] = readInt(splits[1])
        } else if strings.Contains(line, "->") {
            op := ""
            if strings.Contains(line, "XOR") {
                op = " XOR "
            } else if strings.Contains(line, "OR") {
                op = " OR "
            } else {
                op = " AND "
            }
            splits := strings.Split(line, " -> ")
            target := splits[1]
            splits2 := strings.Split(splits[0], op)
            regA := splits2[0]
            regB := splits2[1]
            states[target] = -1
            gates[target] = gate{regA, regB, strings.TrimSpace(op)}
        }
	}
    sum = resolve(states, gates)


	fmt.Printf(" -> Sum: %d\n", sum)

    if err4 := scanner.Err(); err4 != nil {
        fmt.Printf("Error reading input file: %v\n", err4)
        os.Exit(1)
    }

	fmt.Println("Processing complete!")
}
