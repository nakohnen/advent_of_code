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
        fmt.Printf("Error converting str %s to int\n", s)
        os.Exit(1)
    }
    return val
}

const (
    adv int = iota
    bxl
    bst
    jnz
    bxc
    out
    bdv
    cdv
)

// Integer power function
func intPow(base, exp int) int {
	result := 1
	for exp > 0 {
		result *= base
		exp--
	}
	return result
}

func getComboOperand(operand, regA, regB, regC int) int {
    if operand >= 0 && operand < 4 {
        return operand
    }
    switch operand {
        case 4:
            return regA
        case 5:
            return regB
        case 6:
            return regC
        default:
            fmt.Println("Combo operand 7 is reserved and will not appear in valid programs.")
            os.Exit(1)
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

    registerA := 0
    registerB := 0
    registerC := 0

    program := []int{}

    readRegisters := true

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

        if readRegisters {
            if len(line) > 0 {
                parts :=  strings.Split(line, " ")
                middle := parts[1][:1]
                switch middle {
                case "A":
                    registerA = readInt(parts[2])
                case "B":
                    registerB = readInt(parts[2])
                case "C":
                    registerC = readInt(parts[2])
                }
            } else {
                readRegisters = false
            }
        } else {
            if len(line) > 0 {
                parts :=  strings.Split(line, " ")
                if parts[0] == "Program:" {
                    for _, s := range strings.Split(parts[1], ",") {
                        program = append(program, readInt(s))
                    }
                }
            }
        }
	}

    iPointer := 0
    output := []int{}

    fmt.Printf("Registers A: %d, B: %d, C: %d\n", registerA, registerB, registerC)
    fmt.Printf("iPointer: %d\n", iPointer)
    fmt.Printf("Program: %v\n", program)
    fmt.Printf("Output: %v\n", output)

    for iPointer < len(program) {
        opc := program[iPointer]
        operand := program[iPointer+1]
        fmt.Printf("code: %d, operand %d\n", opc, operand)

        iPointerAdd := 2

        switch opc {
        
        case adv: // division
            num := registerA
            combo := getComboOperand(operand, registerA, registerB, registerC)
            denum := intPow(2, combo)
            registerA = num / denum
        case bxl:
            registerB = operand ^ registerB
        case bst:
            combo := getComboOperand(operand, registerA, registerB, registerC)
            registerB = combo % 8
        case jnz:
            if registerA != 0 {
                iPointer = operand
                iPointerAdd = 0
            }
        case bxc:
            registerB = registerB ^ registerC
        case out:
            combo := getComboOperand(operand, registerA, registerB, registerC)
            output = append(output, combo % 8)
        case bdv:
            num := registerA
            combo := getComboOperand(operand, registerA, registerB, registerC)
            denum := intPow(2, combo)
            registerB = num / denum
        case cdv:
            num := registerA
            combo := getComboOperand(operand, registerA, registerB, registerC)
            denum := intPow(2, combo)
            registerC = num / denum
        }
        iPointer += iPointerAdd

        fmt.Printf("Registers A: %d, B: %d, C: %d\n", registerA, registerB, registerC)
        fmt.Printf("iPointer: %d\n", iPointer)
        fmt.Printf("Output: %v\n", output)
    }

    outputString := fmt.Sprintf("%d", output[0])
    for _, out := range output[1:] {
        outputString += fmt.Sprintf(",%d", out)
    }
   
	fmt.Printf(" -> Sum: %v\n", outputString)

    if err4 := scanner.Err(); err4 != nil {
        fmt.Printf("Error reading input file: %v\n", err4)
        os.Exit(1)
    }

	fmt.Println("Processing complete!")
}
