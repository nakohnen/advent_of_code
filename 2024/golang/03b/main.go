package main

import (
	"bufio"
	"fmt"
	"os"
    "strings"
    "strconv"
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

    enable := true

    charList := []rune{')', ',', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'}

    var candidates []string
    
	for scanner.Scan() {
		line := scanner.Text()
        //fmt.Println(line)

        for i, c := range line {
            if i < len(line) - 4 && line[i:i+4] == "do()" {
                enable = true
                fmt.Println(line[i:i+4])
            }

            if i < len(line) - 7 && line[i:i+7] == "don't()"{
                enable = false
                fmt.Println(line[i:i+7])
            }

            if c == 'm' && i <= len(line)-7 && enable {
                if line[i+1] == 'u' && line[i+2] == 'l' && line[i+3] == '(' {
                    start_i := i+4
                    for j := start_i; j<len(line); j++ {

                        if !contains(rune(line[j]), charList) {
                            break /* unvalid mul line */
                        }

                        if line[j] == ')' {
                            /* candidate */
                            candidate := line[start_i:j]
                            candidates = append(candidates, candidate)
                            fmt.Println(fmt.Sprintf("%v", candidate))
                            break
                        }

                    }
                }
            }
        }

	}

    for _, cand := range candidates {
        // Split the string at the comma
        parts := strings.Split(cand, ",")

        // Check if the split resulted in exactly two parts
        if len(parts) != 2 {
            fmt.Println("Error: Input does not contain exactly one comma.")
            return
        }

        // Convert the parts to integers
        num1, err1 := strconv.Atoi(strings.TrimSpace(parts[0]))
        num2, err2 := strconv.Atoi(strings.TrimSpace(parts[1]))

        // Check for conversion errors
        if err1 != nil || err2 != nil {
            fmt.Println("Error: Failed to convert one or both parts to integers.")
            return
        }
        sum += num1 * num2
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
