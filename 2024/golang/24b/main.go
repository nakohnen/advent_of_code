package main

import (
	"bufio"
	"fmt"
	"os"
    //"sync"
    "strings"
    "strconv"
    "sort"
)

// StringSet type using map[string]struct{}
type StringSet struct {
	m map[string]struct{}
}

// NewStringSet creates a new set
func NewStringSet() *StringSet {
	return &StringSet{m: make(map[string]struct{})}
}

// Add adds an element to the set
func (s *StringSet) Add(value string) {
	s.m[value] = struct{}{}
}

// Contains checks if an element is in the set
func (s *StringSet) Contains(value string) bool {
	_, exists := s.m[value]
	return exists
}

// Remove removes an element from the set
func (s *StringSet) Remove(value string) {
	delete(s.m, value)
}

// Size returns the number of elements in the set
func (s *StringSet) Size() int {
	return len(s.m)
}

// GetElements retrieves all elements in the set
func (s *StringSet) GetElements() []string {
	elements := []string{}
	for p := range s.m {
		elements = append(elements, p)
	}
	return elements
}

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
    fmt.Printf("Operator %s not allow in apply (apply(%d, %d, %v))\n", op, a, b, op)
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
    tries := 200
    for len(toCalculate) > 0 {
        current := toCalculate[0]
        toCalculate = toCalculate[1:]

        g := gates[current]
        valA := states[g.regA]
        valB := states[g.regB]
        //fmt.Printf("%s, %s %s %s\n", current, g.regA, g.op, g.regB)
        
        zReg := 0
        if current[0] == 'z' {
            zReg = readInt(current[1:])
            if zReg > maxZ {
                maxZ = zReg
            }
        }

        if valA != -1 && valB != -1 {
            //fmt.Printf("%v %v %v\n", g.regA, g.op, g.regB)
            val := apply(valA, valB, g.op)
            states[current] = val
            if current[0] == 'z' {
                zStates[zReg] = val
            }
            tries = 200
            //fmt.Printf("%v == %v %v %v == %d %v %d == %d\n", current, g.regA, g.op, g.regB, valA, g.op, valB, val)
        } else {
            toCalculate = append(toCalculate, current)
            tries -= 1
            if tries == 0 {
                return -1
            }
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

// ReverseString reverses the input string
func ReverseString(s string) string {
	// Convert the string to a slice of runes to handle Unicode properly
	runes := []rune(s)

	// Reverse the slice of runes
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}

	// Convert the reversed runes back to a string
	return string(runes)
}

func resolvePair(a, b int, gates map[string]gate) (int, map[string]int) {
    states := make(map[string]int)
    maxZ := 0
    for wire, g := range gates {
        states[wire] = -1
        states[g.regA] = -1
        states[g.regB] = -1

        if wire[0] == 'z' {
            zReg := readInt(wire[1:])
            if zReg > maxZ {
                maxZ = zReg
            }
        }
    }
    binA := ReverseString(strconv.FormatInt(int64(a), 2))
    binB := ReverseString(strconv.FormatInt(int64(b), 2))
    for i:=0;i<maxZ;i++{ // Only to maxZ (<) because the last integer is always emtpy
        x := fmt.Sprintf("x%02d", i)
        states[x] = 0
        if i < len(binA) && binA[i] == '1' {
            states[x] = 1
        }

        y :=fmt.Sprintf("y%02d", i)
        states[y] = 0
        if i < len(binB) && binB[i] == '1' {
            states[y] = 1
        }
        //fmt.Printf("%s = %d, %s = %d\n", x, states[x], y, states[y])
    }

    return resolve(states, gates), states
}

// Integer power function
func intPow(base, exp int) int {
	result := 1
	for exp > 0 {
		result *= base
		exp--
	}
	return result
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
    maxZ := 0
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

            if target[0] == 'z' {
                val := readInt(target[1:])
                if val > maxZ {
                    maxZ = val
                }
            }
        }
	}
    allWires := NewStringSet()
    for wire, _ := range gates {
        allWires.Add(wire)
    }
    allWires2 := allWires.GetElements()
    sort.Strings(allWires2)
    for _, wire := range allWires2 {
        fmt.Printf("%s from %s %s %s\n", wire, gates[wire].op, gates[wire].regA, gates[wire].regB)
    }
    
    flip := func(a, b string, gates map[string]gate) {
        tmp := gates[a]
        gates[a] = gates[b]
        gates[b] = tmp
    }



    flip("nfj", "ncd", gates)
    flip("z15", "qnw", gates)
    flip("cqr", "z20", gates)
    flip("z37", "vkg", gates)
    flips := []string{"nfj", "ncd", "z15", "qnw", "cqr", "z20", "z37", "vkg"}
    sort.Strings(flips)

    problemWires := NewStringSet()
    okWires := NewStringSet()
    for i:=-1;i<maxZ;i++ {
        for j:=-1;j<maxZ;j++ {
            for k := -1; k < j;k++ {
                valA := 0
                valB := 0
                valK := 0
                if i >= 0 {
                    valA = intPow(2,i)
                }
                
                if k >= 0 {
                    valK = intPow(2, k)
                }

                if j >= 0 {
                    valB = intPow(2, j) + valK
                }
                res, finStates := resolvePair(valA, valB, gates)
                binaryRes := ReverseString(strconv.FormatInt(int64(res), 2))
                binaryAB:= ReverseString(strconv.FormatInt(int64(valA + valB), 2))
                binaryA:= ReverseString(strconv.FormatInt(int64(valA), 2))
                binaryB:= ReverseString(strconv.FormatInt(int64(valB), 2))
                if valA + valB != res {
                    fmt.Printf("%v (2**%d) + %v (2**%d + 2**%d) =? %v with bx%s (target: %d with bx%s)\n", valA, i, valB, j, k, res, binaryRes, valA + valB, binaryAB)
                    fmt.Printf("bx%s + bx%s == bx%s (target: bx%s)\n", binaryA, binaryB, binaryRes, binaryAB)
                    delta := res - valA - valB
                    binary := ReverseString(strconv.FormatInt(int64(delta), 2))
                    fmt.Printf("Found error: %d (%v highest bit: %d)\n", delta, binary, len(binary))
                    zStates := ""
                    for wire, val := range finStates {
                        if val != 0 {
                            if wire[0] == 'z' {
                                zStates = zStates + fmt.Sprintf(" %s=%d;", wire, val)
                            }
                            if wire[0] != 'z' && wire[0] != 'x' && wire[0] != 'y' {
                                problemWires.Add(wire)
                            }
                        }
                    }
                    if len(zStates) > 0 {
                        fmt.Println(zStates)
                    }
                } else {
                    for wire, val := range finStates {
                        if val != 0 {
                            //fmt.Printf("%s, %d\n", wire, val)
                            if wire[0] != 'z' && wire[0] != 'x' && wire[0] != 'y' {
                                okWires.Add(wire)
                            }
                        }
                    }
                }
            }
        }
    }
    fmt.Printf("Problem wires: %v (%d)\n", problemWires.GetElements(), problemWires.Size())
    //fmt.Printf("OK wires: %v (%d)\n", okWires.GetElements(), okWires.Size())

    okerWires := NewStringSet()
    for _, wire := range problemWires.GetElements() {
        if !okWires.Contains(wire) {
            fmt.Printf("wire %v == (%s %s %s)\n", wire, gates[wire].regA, gates[wire].op, gates[wire].regB)
            okerWires.Add(wire)
        }
    }

    for _, wire := range okerWires.GetElements() {
        g := gates[wire]
        if !okerWires.Contains(g.regA) && !okerWires.Contains(g.regB) {
            fmt.Printf("%v = %s %s %s (not included)\n", wire, g.regA, g.op, g.regB)
        } else {
            fmt.Printf("%v = %s %s %s (included)\n", wire, g.regA, g.op, g.regB)
        }
    }
    debug := false
    if debug {
        cases := []string{"z15", "gnc", "ctg", "rjm", "srr", "qnw", "mrm", "dnn"}
        for m:=0;m<len(cases);m++{
            for n:=m+1;n<len(cases);n++{
                newGates := make(map[string]gate)
                switchA := cases[m]
                switchB := cases[n]
                for key, val := range gates {
                    newGates[key] = val
                }
                flip(switchB, switchA, newGates)
                gateA := gates[switchA]
                gateB := gates[switchB]
                if gateA.regA == switchB || gateA.regB == switchB || gateB.regA == switchA || gateB.regB == switchA {
                    continue
                }

                fmt.Printf("*** Switch %s with %s\n", switchA, switchB)
                errors := 0
                for i:=-1;i<maxZ;i++ {
                    for j:=-1;j<maxZ;j++ {
                        for k := j-2; k < j;k++ {
                            valA := 0
                            valB := 0
                            valK := 0
                            if i >= 0 {
                                valA = intPow(2,i)
                            }

                            if k >= 0 {
                                valK = intPow(2, k)
                            }

                            if j >= 0 {
                                valB = intPow(2, j) + valK
                            }
                            res, _ := resolvePair(valA, valB, newGates)
                            //res, finStates := resolvePair(valA, valB, newGates)
                            //binaryRes := ReverseString(strconv.FormatInt(int64(res), 2))
                            //binaryAB:= ReverseString(strconv.FormatInt(int64(valA + valB), 2))
                            if valA + valB != res {
                                errors += 1
                                //fmt.Printf("%v (2**%d) + %v (2**%d + 2**%d) =? %v with bx%s (target: %d with bx%s)\n", valA, i, valB, j, k, res, binaryRes, valA + valB, binaryAB)
                                //delta := res - valA - valB
                                //binary := ReverseString(strconv.FormatInt(int64(delta), 2))
                                //fmt.Printf("Found error: %d (%v highest bit: %d)\n", delta, binary, len(binary))
                                /**for wire, val := range finStates {
                                    if val != 0 {
                                        if wire[0] == 'z' {
                                            //fmt.Printf("%s, %d\n", wire, val)
                                        }
                                    }
                                }**/
                            }
                        }
                    }
                }
                fmt.Printf("Total Errors: %d\n", errors)
                if errors == 0 {
                    fmt.Printf("Found correct pair")
                }

            }
        }
    }

    fmt.Printf("=== %v\n", strings.Join(flips, ","))

	fmt.Printf(" -> Sum: %d\n", sum)

    if err4 := scanner.Err(); err4 != nil {
        fmt.Printf("Error reading input file: %v\n", err4)
        os.Exit(1)
    }

	fmt.Println("Processing complete!")
}
