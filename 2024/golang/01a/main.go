package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
    "sort"
)

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

    var leftSide []int
    var rightSide []int
    sum := 0
    
	for scanner.Scan() {
		line := scanner.Text()
        //fmt.Println(line)

		parts := strings.Fields(line) // Split line by whitespace
        if len(parts) == 2 {
            num1, err1 := strconv.Atoi(parts[0])
            if err1 != nil {
                fmt.Printf("Skipping non-numeric value: %v\n", parts[0])
                continue
            }
            leftSide = append(leftSide, num1)


            num2, err2 := strconv.Atoi(parts[1])
            if err2 != nil {
                fmt.Printf("Skipping non-numeric value: %v\n", parts[1])
                continue
            }
            rightSide = append(rightSide, num2)
        } else {
            fmt.Println(line)
        }

	}
    sort.Ints(leftSide)
    sort.Ints(rightSide)


    // Loop over the slices using an index
    for i := 0; i < len(leftSide); i++ {
        val := leftSide[i] - rightSide[i]
        if val < 0 {    
            sum += -val
        }           else {
            sum += val
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
