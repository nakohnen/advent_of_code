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
	blockLengths := make(map[int]int)
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
				blockLengths[runningId] = number
				runningId += 1
			}
			free = !free
		}
	}

	for i := len(diskMap) - 1; i >= 0; i-- {
		if diskMap[i] == -1 {
			continue
		}
		blockLength := blockLengths[diskMap[i]]
		blockStart := i - blockLength + 1
		if diskMap[blockStart] != diskMap[i] {
			fmt.Printf("Error on an invariant: %v %v %v %v %d\n", blockStart, diskMap[blockStart], i, diskMap[i], blockLength)
			os.Exit(1)
		}

		// Search for empty space in front
		for j := 0; j < blockStart; j++ {
			if diskMap[j] != -1 {
				continue
			}
			// We found an empty space
			// Check if enough space
			emptyStart := j
			emptyEnd := j
			for k := j + 1; k < blockStart; k++ {
				if diskMap[k] != -1 {
					emptyEnd = k - 1
					break
				}
			}
			emptyLength := emptyEnd - emptyStart + 1

			// We have enough space
			if emptyLength >= blockLength {
				// Place the block
				for l := 0; l < blockLength; l++ {
					diskMap[emptyStart+l] = diskMap[i]
					diskMap[blockStart+l] = -1
				}
				break
			}

			// We dont have enough space
			// Continue at the next block
			j = emptyEnd
		}
		i = blockStart
	}
	fmt.Printf("%v\n", diskMap)
	for i, val := range diskMap {
		if val >= 0 {
			result := i * val
			sum += result
			//fmt.Printf("%d * %d = %d -> sum=%d\n", i, val, result, sum)
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
