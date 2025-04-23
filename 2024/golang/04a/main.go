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

	charList := []byte{'X', 'M', 'A', 'S'}
    xmas_len := len(charList) - 1

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
			fmt.Println(fmt.Sprintf("Coord: (%v, %v)", x, y))
			found := 0
			// Upper left
			if x-xmas_len >= 0 &&
				y-xmas_len >= 0 &&
				inputText[y][x] == charList[0] &&
				inputText[y-1][x-1] == charList[1] &&
				inputText[y-2][x-2] == charList[2] &&
				inputText[y-3][x-3] == charList[3] {
				fmt.Println("Up left")
				found += 1
			}
			// Up
			if y-xmas_len >= 0 &&
				inputText[y][x] == charList[0] &&
				inputText[y-1][x] == charList[1] &&
				inputText[y-2][x] == charList[2] &&
				inputText[y-3][x] == charList[3] {
				fmt.Println("Up")
				found += 1
			}
			// Up right
			if y-xmas_len >= 0 &&
				x+xmas_len < maxWidth &&
				inputText[y][x] == charList[0] &&
				inputText[y-1][x+1] == charList[1] &&
				inputText[y-2][x+2] == charList[2] &&
				inputText[y-3][x+3] == charList[3] {
				fmt.Println("Up right")
				found += 1
			}
			// Left
			if x-xmas_len >= 0 &&
				inputText[y][x] == charList[0] &&
				inputText[y][x-1] == charList[1] &&
				inputText[y][x-2] == charList[2] &&
				inputText[y][x-3] == charList[3] {
				fmt.Println("Left")
				found += 1
			}
			// Right
			if x+xmas_len < maxWidth &&
				inputText[y][x] == charList[0] &&
				inputText[y][x+1] == charList[1] &&
				inputText[y][x+2] == charList[2] &&
				inputText[y][x+3] == charList[3] {
				fmt.Println("Right")
				found += 1
			}
			// Lower Left
			if x-xmas_len >= 0 &&
				y+xmas_len < maxHeight &&
				inputText[y][x] == charList[0] &&
				inputText[y+1][x-1] == charList[1] &&
				inputText[y+2][x-2] == charList[2] &&
				inputText[y+3][x-3] == charList[3] {
				fmt.Println("Down left")
				found += 1
			}
			// Down
			if y+xmas_len < maxHeight &&
				inputText[y][x] == charList[0] &&
				inputText[y+1][x] == charList[1] &&
				inputText[y+2][x] == charList[2] &&
				inputText[y+3][x] == charList[3] {
				fmt.Println("Down")
				found += 1
			}
			// Lower Right
			if y+xmas_len < maxHeight &&
				x+xmas_len < maxWidth &&
				inputText[y][x] == charList[0] &&
				inputText[y+1][x+1] == charList[1] &&
				inputText[y+2][x+2] == charList[2] &&
				inputText[y+3][x+3] == charList[3] {
				fmt.Println("Down right")
				found += 1
			}

			if found > 0 {
				sum += found

				upper_left_x := max(0, x-xmas_len)
				upper_left_y := max(0, y-xmas_len)
				lower_right_x := min(maxWidth, x+xmas_len)
				lower_right_y := min(maxHeight, y+xmas_len)

				fmt.Println(fmt.Sprintf("Found %v - (%v,%v) (%v,%v)", found, upper_left_x, upper_left_y, lower_right_x, lower_right_y))
				for y2 := upper_left_y; y2 < lower_right_y; y2++ {
					fmt.Println(inputText[y2][upper_left_x:lower_right_x])
				}
				fmt.Println("---------------------------")

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
