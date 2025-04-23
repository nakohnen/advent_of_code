package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
)

func readInt(s string) int {
	val, err := strconv.Atoi(s)
	if err != nil {
		fmt.Printf("Error converting str %s to int\n", s)
		os.Exit(1)
	}
	return val
}

func asOctal(num int) string {
	res := num / 8
	mod := num % 8
	out := fmt.Sprintf("%d", mod)
	for res > 0 {
		res = res / 8
		mod = res % 8
		if res > 0 {
			out = fmt.Sprintf("%d", mod) + out
		}
	}
	return "o" + out
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

func run(program []int, inA, inB, inC int) (int, []int) {
	iPointer := 0
	output := []int{}

	regA := inA
	regB := inB
	regC := inC

	fmt.Printf("Register A := %d (octal %s)\n", regA, asOctal(regA))
	fmt.Printf("Register B := %d (octal %s)\n", regB, asOctal(regB))
	fmt.Printf("Register C := %d (octal %s)\n", regC, asOctal(regC))

	for iPointer < len(program) {
		opc := program[iPointer]
		operand := program[iPointer+1]
		//fmt.Printf("code: %d, operand %d\n", opc, operand)

		iPointerAdd := 2
		fmt.Printf("%d: ", iPointer)
		switch opc {
		case adv: // division
			num := regA
			combo := getComboOperand(operand, regA, regB, regC)
			denum := intPow(2, combo)
			div := num / denum
			fmt.Printf("adv => register A := register A / 2^combo (%s = %s / %s with combo %s)\n", asOctal(div), asOctal(num), denum, asOctal(combo))
			regA = div
		case bxl:
			res := operand ^ regB
			fmt.Printf("bxl => register B := literal operand XOR register B (%s = %s XOR %s)\n", asOctal(res), asOctal(operand), asOctal(regB))
			regB = res
		case bst:
			combo := getComboOperand(operand, regA, regB, regC)
			res := combo % 8
			fmt.Printf("bst => register B := combo mod 8 (%s = %s mod 8)\n", asOctal(res), asOctal(combo))
			regB = res
		case jnz:
			if regA != 0 {
				fmt.Printf("jnz => Jumping from %d to %d\n", iPointer, operand)
				iPointer = operand
				iPointerAdd = 0
			} else {
				fmt.Println("jnz => Jumping ignored as register A is zero")
			}
		case bxc:
			res := regB ^ regC
			fmt.Printf("bxc => register B := register B XOR register C (%s = %s XOR %s)\n", asOctal(res), asOctal(regB), asOctal(regC))
			regB = res
		case out:
			combo := getComboOperand(operand, regA, regB, regC)
			res := combo % 8
			fmt.Printf("out => output := combo mod 8 (%s = %s mod 8)\n", asOctal(res), asOctal(combo))
			output = append(output, res)
		case bdv:
			num := regA
			combo := getComboOperand(operand, regA, regB, regC)
			denum := intPow(2, combo)
			res := num / denum
			fmt.Printf("bdv => register B := register A / 2^combo (%s = %s / %s with combo %s)\n", asOctal(res), asOctal(num), denum, asOctal(combo))
			regB = res
		case cdv:
			num := regA
			combo := getComboOperand(operand, regA, regB, regC)
			denum := intPow(2, combo)
			res := num / denum
			fmt.Printf("adv => register C := register A / 2^combo (%s = %s / %s with combo %s)\n", asOctal(res), asOctal(num), denum, asOctal(combo))
			regC = res
		}
		iPointer += iPointerAdd

		//fmt.Printf("Registers A: %d, B: %d, C: %d\n", regA, regB, regC)
		//fmt.Printf("iPointer: %d\n", iPointer)
		//fmt.Printf("Output: %v\n", output)

		//reader := bufio.NewReader(os.Stdin)
		//fmt.Println("Press enter to continue")
		//l,_ := reader.ReadString('\n') // Reads until a newline
		//fmt.Println(l)

	}
	fmt.Printf("Final Output: %v\n", output)
	return inA, output
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

	//powerNumber := intPow(2,18)
	registerA = 100000000
	//registerA = 251359562578959
	//registerA = 281474977000000
	registerA = intPow(8, 16) - 1
	//_, o := run(program, registerA, registerB, registerC)
	//for len(o) <= len(program) {
	//    registerA += 1000000
	//    _, o = run(program, registerA, registerB, registerC)
	//}
	sum := 0
	exit := false
	block := intPow(8, 4)

	fmt.Printf("RegisterA: %d (octal %s)\n", registerA, asOctal(registerA))

	// Worker function closure that uses `multiplier`
	worker := func(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
		defer wg.Done()

		for regA := range jobs {
			result, out := run(program, regA, registerB, registerC)
			//fmt.Printf("Worker %d processed job: %d -> %d\n", id, job, result)
			foundDelta := false
			//if len(out) > len(program) {
			//    fmt.Printf("Job %d, Program %v, out %v\n", regA, program, out)
			//    //os.Exit(1)
			//}
			if out[15] == 0 {
				fmt.Printf("Job %d, Program %v, out %v\n", regA, program, out)
			}
			for i := 0; i < len(out); i++ {
				if out[i] != program[i] {
					if i > 13 {
						fmt.Printf("Job %d, Program %v, out %v, delta on %d\n", regA, program, out, i)
					}
					foundDelta = true
					break
				}
			}
			if len(out) != len(program) {
				foundDelta = true
			}
			if !foundDelta {
				results <- result
			} else {
				results <- -1
			}
		}
	}

	for !exit {

		// Inputs and setup
		numWorkers := 1

		jobs := make(chan int, block)
		results := make(chan int, block)
		var wg sync.WaitGroup

		// Start workers
		for i := 1; i <= numWorkers; i++ {
			wg.Add(1)
			go worker(i, jobs, results, &wg)
		}

		// Send jobs
		for i := 0; i < block; i++ {
			jobs <- registerA - i
		}
		close(jobs)

		// Wait and collect results
		wg.Wait()
		close(results)

		minVal := int(^uint(0) >> 1)
		for result := range results {
			if result >= 0 && result < minVal {
				exit = true
				minVal = result
			}
		}

		registerA -= 1000
		if exit {
			sum = minVal
		}
	}

	fmt.Printf(" -> Sum: %v\n", sum)

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
