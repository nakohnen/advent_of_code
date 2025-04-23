package main

import (
	"bufio"
	"fmt"
	"os"
)

type point struct {
	x int
	y int
}

// Generic contains function
func contains[T comparable](slice []T, element T) bool {
	for _, item := range slice {
		if item == element {
			return true
		}
	}
	return false
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

	y := 0

	antennas := make(map[rune][]point)
	var antennaTypes []rune
	var allAntennas []point

	maxWidth := 0
	maxHeight := 0

	var antinodes []point

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

		for x, char := range line {
			r := rune(char)
			if r != '.' {
				p := point{x, y}
				allAntennas = append(allAntennas, p)
				if !contains(antennaTypes, r) {
					antennaTypes = append(antennaTypes, r)
				}
				_, exists := antennas[r]
				if !exists {
					antennas[r] = []point{p}
				} else {
					antennas[r] = append(antennas[r], p)
				}
			}
			if x >= maxWidth {
				maxWidth = x + 1
			}
		}

		if y >= maxHeight {
			maxHeight = y + 1
		}

		y += 1

	}
	fmt.Printf("maxWidth %v, maxHeight %v\n", maxWidth, maxHeight)
	fmt.Printf("Total antennas: %v (%v)\n", len(allAntennas), allAntennas)
	fmt.Printf("Total antenna types: %v\n", len(antennaTypes))
	fmt.Printf("Antennas %v\n", antennas)

    for _, ant := range allAntennas {
        antinodes = append(antinodes, ant)
    }

	for _, ant := range antennaTypes {
		antennas := antennas[ant]

		for i := 0; i < len(antennas); i++ {
			antenna1 := antennas[i]
			for j := i + 1; j < len(antennas); j++ {
				antenna2 := antennas[j]
				x1 := antenna1.x - antenna2.x
				x2 := antenna2.x - antenna1.x
				y1 := antenna1.y - antenna2.y
				y2 := antenna2.y - antenna1.y

                newAntinodes := []point{}
                
                antinode1 := point{x: antenna1.x + x1, y: antenna1.y + y1}
                for antinode1.x >= 0 && antinode1.x < maxWidth &&
                antinode1.y >= 0 && antinode1.y < maxHeight {
                    if !contains(antinodes, antinode1) {
                        antinodes = append(antinodes, antinode1)
                        newAntinodes = append(newAntinodes, antinode1)
                    }
                    antinode1 = point{x: antinode1.x + x1, y: antinode1.y + y1}
                }

                antinode2 := point{x: antenna2.x + x2, y: antenna2.y + y2}
                for antinode2.x >= 0 && antinode2.x < maxWidth &&
                antinode2.y >= 0 && antinode2.y < maxHeight {
                    if !contains(antinodes, antinode2) {
                        antinodes = append(antinodes, antinode2)
                        newAntinodes = append(newAntinodes, antinode2)
                    }
                    antinode2 = point{x: antinode2.x + x2, y: antinode2.y + y2}
                }
				fmt.Printf("%v %v <-> %v\n", ant, antenna1, antenna2)
				fmt.Printf(" -> %v\n", newAntinodes)

			}
		}

	}

    sum = len(antinodes)

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
