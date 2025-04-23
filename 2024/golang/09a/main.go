package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
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

	sum := 0

	diskMap := []int{}
	runningId := 0
	free := false

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

		for _, count := range line {
			number, err := strconv.Atoi(string(count))
			if err != nil {
				fmt.Println("Error converting numbers.")
				os.Exit(1)
			}

			for j := 0; j < number; j++ {
				id := -1
				if !free {
					id = runningId
				}
				diskMap = append(diskMap, id)
			}
			if !free {
				runningId += 1
			}
			free = !free
		}
	}

	lastEndPosition := len(diskMap) - 1
	for i := 0; i < len(diskMap); i++ {
		if lastEndPosition < i {
			break
		}
		if diskMap[i] >= 0 {
			continue
		}

		target := lastEndPosition
		for j := lastEndPosition; j >= 0; j-- {
			if diskMap[j] == -1 {
				continue
			} else {
				target = j
				break
			}
		}
		lastEndPosition = target - 1
		diskMap[i] = diskMap[target]
		diskMap[target] = -1

	}

	for i, val := range diskMap {
		if val >= 0 {
			result := i * val
			sum += result
			fmt.Printf("%d * %d = %d -> sum=%s\n", i, val, result, sum)
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
