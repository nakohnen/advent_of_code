package main

import (
	"bufio"
	"fmt"
	"os"
)

func contains(t int, slice []int) bool {
	for _, s := range slice {
		if s == t {
			return true
		}
	}
	return false
}

// Define a custom struct
type point struct {
	x int
	y int
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

	obstacleMap := make(map[point]bool)
	visitedMap := make(map[point]bool)
	var guardPosition point
	guardDirection := 'U'

	y := 0

	maxHeight := 0
	maxWidth := 0

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

		for x, char := range line {
			p := point{x, y}
			obstacleMap[p] = false
			visitedMap[p] = false
			if char == '#' {
				obstacleMap[p] = true
			}
            if char == '^' {
				guardPosition = p
				visitedMap[p] = true
			}

			if x > maxWidth {
				maxWidth = x
			}
		}
		if y > maxHeight {
			maxHeight = y
		}

        y += 1
	}

    fmt.Println(fmt.Sprintf("maxWidth maxHeight %v %v", maxWidth, maxHeight))
	left := false
	for !left {
        fmt.Println(fmt.Sprintf("Current position and direction %v %s", guardPosition, string(guardDirection) ))

		nextPosition := guardPosition
		nextDirection := guardDirection
		if guardDirection == 'U' {
			nextPosition.y += -1
			nextDirection = 'R'
		} else if guardDirection == 'R' {
			nextPosition.x += 1
			nextDirection = 'D'
		} else if guardDirection == 'D' {
			nextPosition.y += 1
			nextDirection = 'L'
		} else {
			nextPosition.x += -1
			nextDirection = 'U'
		}
		if nextPosition.x < 0 || nextPosition.x > maxWidth || nextPosition.y < 0 ||
			nextPosition.y > maxHeight {
			left = true
			break
		}

		if obstacleMap[nextPosition] {
			guardDirection = nextDirection
		} else {
			visitedMap[nextPosition] = true
			guardPosition = nextPosition
		}
	}

	for x1 := 0; x1 <= maxWidth; x1++ {
		for y1 := 0; y1 <= maxHeight; y1++ {
			if visitedMap[point{x1, y1}] {
				sum += 1
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
