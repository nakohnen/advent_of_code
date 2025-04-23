package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
    "errors"
)

func abs(i int) int {
    if i < 0 {
        return -i
    }
    return i
}

// max calculates the maximum value in a slice of integers.
// Returns an error if the slice is empty.
func max(slice []int) (int, error) {
	// Check for an empty slice
	if len(slice) == 0 {
		return 0, errors.New("slice is empty, cannot determine maximum")
	}

	// Initialize max as the first element
	maxValue := slice[0]

	// Loop through the slice to find the maximum value
	for _, value := range slice {
		if value > maxValue {
			maxValue = value
		}
	}

	return maxValue, nil // Return max value with no error
}

func countSigns(slice []int, zero_inclusive bool) (int, int) {
    negs, pos := 0, 0
    if len(slice) == 0 {
        return pos, negs
    }

    for _, val := range slice {
        if val < 0 {
            negs += 1
        } else if val == 0 && zero_inclusive {
            pos += 1
        } else if val > 0 {
            pos += 1
        }
    }
    return pos, negs

}

func countFor(slice []int, target int) int {
    if len(slice) == 0 {
        return 0
    }
    t := 0
    for _, val := range slice {
        if val == target {
            t += 1
        }
    }
    return t
}

// xor returns the XOR result of two boolean values.
func xor(a, b bool) bool {
	return (a || b) && !(a && b)
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
    
	for scanner.Scan() {
		line := scanner.Text()
        //fmt.Println(line)

		parts := strings.Fields(line) // Split line by whitespace


        var report []int

		for _, part := range parts {
			num, err := strconv.Atoi(part)
			if err != nil {
				fmt.Printf("Skipping non-numeric value: %v\n", part)
				continue
			}
            report = append(report, num)
		}


        for i := -1; i < len(report); i++ {
            var subreport []int
            
            for j, val := range report {
                if i != j {
                    subreport = append(subreport, val)
                }
            }

            safe := true
            var seq []int
            var abs_seq []int

            last_num := 0
            for j, num := range subreport {
                if j > 0 {
                    delta := num - last_num
                    seq = append(seq, delta)
                    abs_seq = append(abs_seq, abs(delta))
                }
                last_num = num
            }

            max_delta, err := max(abs_seq)
            if err != nil {
                fmt.Printf("We got an error with line: %v\n", report)
                continue
            }

            rise, fall := countSigns(seq, false)
            is_rising := rise != 0
            is_falling := fall != 0
            has_stagnant := countFor(seq, 0) > 0
            not_xor_same := !xor(is_rising, is_falling)

            if max_delta > 3 || not_xor_same || has_stagnant {
                safe = false
            }

            fmt.Println(fmt.Sprintf("%v, %v, %v - %v, %v, %v, %v, %v, %v", subreport, seq, abs_seq, max_delta, is_rising, is_falling, not_xor_same, has_stagnant, safe))
            if safe {
                fmt.Println(fmt.Sprintf("Safe: %v", line))
                sum += 1
                break
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
