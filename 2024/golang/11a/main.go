package main

import (
	"bufio"
	"fmt"
	"os"
	//"sync"
	"strconv"
	"strings"
)

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

	stones := []int{}

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

		for _, part := range strings.Split(line, " ") {
			val, err := strconv.Atoi(part)
			if err != nil {
				fmt.Printf("Error converting number %v\n", part)
				os.Exit(1)
			}
			stones = append(stones, val)
		}
	}

	fmt.Printf("Stones: %v\n", stones)
	for i := 0; i < 25; i++ {
		newStones := []int{}

		for _, stone := range stones {
			if stone == 0 {
				newStones = append(newStones, 1)
				continue
			}
			digits := fmt.Sprintf("%d", stone)
			if len(digits)%2 == 0 {
				val1, err := strconv.Atoi(digits[:len(digits)/2])
				if err != nil {
					fmt.Printf("Error converting number %s\n", digits)
					os.Exit(1)
				}
				val2, err := strconv.Atoi(digits[len(digits)/2:])
				if err != nil {
					fmt.Printf("Error converting number %s\n", digits)
					os.Exit(1)
					os.Exit(1)
				}
				newStones = append(newStones, val1)
				newStones = append(newStones, val2)
				continue
			}
			newStones = append(newStones, stone*2024)
		}
		stones = newStones
		if len(stones) < 22 {
			fmt.Printf("Stones: %v\n", stones)
		} else {

			fmt.Printf("Stones count: %v\n", len(stones))
		}
	}

	sum = len(stones)

	fmt.Printf(" -> Sum: %d\n", sum)

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
