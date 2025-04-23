package main

import (
	"bufio"
	"fmt"
	"os"
	//"sync"
	"strconv"
	"strings"
)

func readInt(s string) int {
	val, err := strconv.Atoi(s)
	if err != nil {
		//fmt.Printf("Error converting str %s to int\n", s)
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

func replaceAtIndex(input string, index int, replacement rune) string {
	// Convert the string to a slice of runes (to handle multi-byte characters)
	runes := []rune(input)

	// Replace the character at the given index
	runes[index] = replacement

	// Convert the rune slice back to a string
	return string(runes)
}

func asOctal(num int) string {
	return "o" + fmt.Sprintf("%o", num)
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
		//fmt.Println("Combo operand 7 is reserved and will not appear in valid programs.")
		os.Exit(1)
	}
	return 0
}

func getCombo(operand, regA, regB, regC int) int {
	c := getComboOperand(operand, regA, regB, regC)
	//fmt.Printf("Combo operand: %d\n", c)
	return c
}

func run(program []int, inA, inB, inC int) (int, []int) {
	iPointer := 0
	output := []int{}

	regA := inA
	regB := inB
	regC := inC

	fmt.Printf("Register A := %d (octal %s)\n", regA, asOctal(regA))
	//fmt.Printf("Register B := %d (octal %s)\n", regB, asOctal(regB))
	//fmt.Printf("Register C := %d (octal %s)\n", regC, asOctal(regC))

	for iPointer < len(program) {
		opc := program[iPointer]
		operand := program[iPointer+1]
		////fmt.Printf("code: %d, operand %d\n", opc, operand)

		iPointerAdd := 2
		//fmt.Printf("%d: ", iPointer)
		switch opc {
		case adv: // division
			num := regA
			combo := getComboOperand(operand, regA, regB, regC)
			denum := intPow(2, combo)
			div := num / denum
			//fmt.Printf("adv => register A := register A / 2^combo (%s = %s / %d with combo %s)\n", asOctal(div), asOctal(num), denum, asOctal(combo))
			regA = div
		case bxl:
			res := operand ^ regB
			//fmt.Printf("bxl => register B := literal operand XOR register B (%s = %s XOR %s)\n", asOctal(res), asOctal(operand), asOctal(regB))
			regB = res
		case bst:
			combo := getComboOperand(operand, regA, regB, regC)
			res := combo % 8
			//fmt.Printf("bst => register B := combo mod 8 (%s = %s mod 8)\n", asOctal(res), asOctal(combo))
			regB = res
		case jnz:
			if regA != 0 {
				//fmt.Printf("jnz => Jumping from %d to %d\n", iPointer, operand)
				iPointer = operand
				iPointerAdd = 0
                //fmt.Printf("Register A := %d (octal %s)\n", regA, asOctal(regA))
                //fmt.Printf("Register B := %d (octal %s)\n", regB, asOctal(regB))
                //fmt.Printf("Register C := %d (octal %s)\n", regC, asOctal(regC))
                //fmt.Println("")
			} else {
				//fmt.Println("jnz => Jumping ignored as register A is zero")
			}
		case bxc:
			res := regB ^ regC
			//fmt.Printf("bxc => register B := register B XOR register C (%s = %s XOR %s)\n", asOctal(res), asOctal(regB), asOctal(regC))
			regB = res
		case out:
			combo := getComboOperand(operand, regA, regB, regC)
			res := combo % 8
			//fmt.Printf("out => output := combo mod 8 (%s = %s mod 8)\n", asOctal(res), asOctal(combo))
			output = append(output, res)
		case bdv:
			num := regA
			combo := getComboOperand(operand, regA, regB, regC)
			denum := intPow(2, combo)
			res := num / denum
			//fmt.Printf("bdv => register B := register A / 2^combo (%s = %s / %d with combo %s)\n", asOctal(res), asOctal(num), denum, asOctal(combo))
			regB = res
		case cdv:
			num := regA
			combo := getComboOperand(operand, regA, regB, regC)
			denum := intPow(2, combo)
			res := num / denum
			//fmt.Printf("adv => register C := register A / 2^combo (%s = %s / %d with combo %s)\n", asOctal(res), asOctal(num), denum, asOctal(combo))
			regC = res
		}
		iPointer += iPointerAdd

		////fmt.Printf("Registers A: %d, B: %d, C: %d\n", regA, regB, regC)
		////fmt.Printf("iPointer: %d\n", iPointer)
		////fmt.Printf("Output: %v\n", output)

		//reader := bufio.NewReader(os.Stdin)
		////fmt.Println("Press enter to continue")
		//l,_ := reader.ReadString('\n') // Reads until a newline
		////fmt.Println(l)

	}
	//fmt.Printf("Final Output: %v\n", output)
	return inA, output
}

func main() {
	// Check if enough arguments are provided
	//if len(os.Args) != 3 {
	if len(os.Args) != 2 {
		//fmt.Println("Usage: go run main.go <input_file>")
		os.Exit(1)
	}

	// Parse command-line arguments
	inputFile := os.Args[1]

	//inputA := os.Args[2]

	// Open the input file
	inFile, err := os.Open(inputFile)
	if err != nil {
		//fmt.Printf("Error opening input file: %v\n", err)
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
		////fmt.Println(line)

		if readRegisters {
			if len(line) > 0 {
				parts := strings.Split(line, " ")
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
				parts := strings.Split(line, " ")
				if parts[0] == "Program:" {
					for _, s := range strings.Split(parts[1], ",") {
						program = append(program, readInt(s))
					}
				}
			}
		}
	}
    
    tries := []string{}
    tries = append(tries, "7000000000000000")
    nextTries := []string{}
    results := []int{}
    for pos:= 0; pos < 16; pos++ {
        for len(tries) > 0 {
            fmt.Printf("Pos %d\n", pos)
            if pos >= 16 {
                break
            }
            octal := tries[0]
            tries = tries[1:]

            fmt.Println(octal)
            for o:=0;o<8;o++ {
                replacement := fmt.Sprintf("%d", o)
                octal2 := replaceAtIndex(octal, pos, rune(replacement[0]))
                decimal, err := strconv.ParseInt(octal2, 8, 64)
                if err != nil {
                    //fmt.Println("Error:", err)
                    os.Exit(1)
                }

                registerA = int(decimal)

                //fmt.Printf("Program: %v\n", program)

                _, output := run(program, registerA, registerB, registerC)
                fmt.Printf("%v %v\n", program, output)

                found := false
                if len(output) < len(program) {
                    found = true 
                } else {
                    for i:=len(program)-1; i >= len(program)-1 - pos; i-- {
                        j := i
                        if output[j] != program[j]{
                            found = true
                            break
                        }
                    }
                }
                if !found {
                    fmt.Print("Added ")
                    fmt.Println(octal2)
                    nextTries = append(nextTries, octal2)
                }
            }
        }
        tries = nextTries
        nextTries = []string{}
    }

    for _, res := range tries {
        decimal, err := strconv.ParseInt(res, 8, 64)
        if err != nil {
            //fmt.Println("Error:", err)
            os.Exit(1)
        }

        results = append(results, int(decimal))
    }

    sum := int(^uint(0) >> 1)
    for _, res := range results {
        if res < sum {
            sum = res
        }
    }

	fmt.Printf("Program: %v\n", program)
    fmt.Printf("Results %v\n", results)
    fmt.Printf("--> Sum: %d\n", sum)

	if err4 := scanner.Err(); err4 != nil {
		//fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	//fmt.Println("Processing complete!")
}
