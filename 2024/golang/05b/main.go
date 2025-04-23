package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func contains(t int, slice []int) bool {
	for _, s := range slice {
		if s == t {
			return true
		}
	}
	return false
}

func checkIfValid(intSlice []int, precedes map[int][]int) (bool, int, int) {
    for i := 1; i < len(intSlice); i++ {
        current := intSlice[i]
        precedesSlice, exists := precedes[current]
        if !exists {
            precedesSlice = []int{}
        }
        fmt.Println(fmt.Sprintf("Checking %v|%v", current, precedesSlice))
        for j, prev := range intSlice[:i] {
            if contains(prev, precedesSlice) {
                return false, i, j
            }
        }

    }
    return true, 0, 0
}

func main() {
	// Check if enough arguments are provided
	if len(os.Args) != 3 {
		fmt.Println("Usage: go run script.go <input_file> <output_file>")
		os.Exit(1)
	}

	// Parse command-line arguments
	inputFile := os.Args[1]
	outputFile := os.Args[2]

	// Open the input file
	inFile, err := os.Open(inputFile)
	if err != nil {
		fmt.Printf("Error opening input file: %v\n", err)
		os.Exit(1)
	}
	defer inFile.Close()

	// Create the output file
	outFile, err := os.Create(outputFile)
	if err != nil {
		fmt.Printf("Error creating output file: %v\n", err)
		os.Exit(1)
	}
	defer outFile.Close()

	// Prepare for line-by-line reading and writing
	scanner := bufio.NewScanner(inFile)
	writer := bufio.NewWriter(outFile)

	sum := 0

	precedes := make(map[int][]int)

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

		if strings.ContainsRune(line, '|') {
			parts := strings.Split(string(line), "|")

			if len(parts) != 2 {
				fmt.Println("Error processing string split")
				os.Exit(1)
			}

			leftSide, err := strconv.Atoi(parts[0])
			if err != nil {
				fmt.Println("Error processing string split conversion")
				os.Exit(1)
			}
			rightSide, err := strconv.Atoi(parts[1])
			if err != nil {
				fmt.Println("Error processing string split conversion")
				os.Exit(1)
			}

			precedesSlice, exists := precedes[leftSide]
			if exists {
				precedesSlice = append(precedesSlice, rightSide)
				precedes[leftSide] = precedesSlice
			} else {
				precedes[leftSide] = []int{rightSide}
			}
		} else if strings.ContainsRune(line, ',') {
			intSlice := []int{}
			parts := strings.Split(string(line), ",")
			for _, part := range parts {

				cInt, err := strconv.Atoi(part)
				if err != nil {
					fmt.Println("Error processing string split conversion")
					os.Exit(1)
				}
                intSlice = append(intSlice, cInt)
			}
            fmt.Println(line)

			validUpdate, switchHigher, switchLower := checkIfValid(intSlice, precedes)
            oldValid := validUpdate
            fixed := false
            // func checkIfValid(slice []int, precedesSlice map[int][]int) bool {
            for !validUpdate {
                valHigher := intSlice[switchHigher]
                valLower := intSlice[switchLower]

                intSlice[switchLower] = valHigher
                intSlice[switchHigher] = valLower

			    validUpdate, switchHigher, switchLower = checkIfValid(intSlice, precedes)
                if oldValid != validUpdate && validUpdate {
                    fixed = true
                }
            }

			if fixed {
                midPoint := len(intSlice)/2
                midValue := intSlice[midPoint]
                fmt.Println(fmt.Sprintf("Update %v is valid now as %v and we add %v to sum %v", line, intSlice, midValue, sum))
                sum += midValue
			}
		}
	}

	// Write the result to the output file
	output_string := fmt.Sprintf(" -> Sum: %d\n", sum)
	fmt.Println(output_string)

	_, err3 := writer.WriteString(output_string)
	if err3 != nil {
		fmt.Printf("Error writing to output file: %v\n", err3)
		os.Exit(1)
	}

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	// Flush remaining data to the output file
	err5 := writer.Flush()
	if err5 != nil {
		fmt.Printf("Error flushing output file: %v\n", err5)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
