package main

import (
	"bufio"
	"fmt"
	"os"
)

// Function to check if a character is in a slice of runes
func contains(char rune, list []rune) bool {
	for _, c := range list {
		if c == char {
			return true
		}
	}
	return false
}

func max(i1, i2 int) int {
	if i1 >= i2 {
		return i1
	}
	return i2
}

func min(i1, i2 int) int {
	if i1 <= i2 {
		return i1
	}
	return i2
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

	charList := []byte{'M', 'A', 'S'}
	var inputText []string

	maxWidth := 0

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

		if len(line) > 0 {
			inputText = append(inputText, line)

			if maxWidth < len(line) {
				maxWidth = len(line)
			}
		}

	}
	maxHeight := len(inputText)

	fmt.Println(fmt.Sprintf("width: %v, height: %v", maxWidth, maxHeight))

	for y := 0; y < maxHeight; y++ {
		for x := 0; x < maxWidth; x++ {
			if inputText[y][x] == charList[1] &&
				x-1 >= 0 && y-1 >= 0 &&
				x+1 < maxWidth && y+1 < maxHeight {
				fmt.Println(fmt.Sprintf("Coord: (%v, %v)", x, y))
				slash_diag := 0
				backslash_diag := 0

				if inputText[y-1][x-1] == charList[0] &&
					inputText[y+1][x+1] == charList[2] {
					backslash_diag += 1
				}

				if inputText[y-1][x-1] == charList[2] &&
					inputText[y+1][x+1] == charList[0] {
					backslash_diag += 1
				}

				if inputText[y+1][x-1] == charList[0] &&
					inputText[y-1][x+1] == charList[2] {
					slash_diag += 1
				}

				if inputText[y+1][x-1] == charList[2] &&
					inputText[y-1][x+1] == charList[0] {
					slash_diag += 1
				}

				if backslash_diag*slash_diag > 0 {
					sum += 1
				}

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
