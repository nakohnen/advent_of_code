package main

import (
	"bufio"
	"fmt"
	"os"
    //"sync"
    //"strings"
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

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

	}
   
	fmt.Printf(" -> Sum: %d\n", sum)

    if err4 := scanner.Err(); err4 != nil {
        fmt.Printf("Error reading input file: %v\n", err4)
        os.Exit(1)
    }

	fmt.Println("Processing complete!")
}
