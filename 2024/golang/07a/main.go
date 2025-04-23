package main

import (
	"bufio"
	"fmt"
	"os"
    "strings"
    "strconv"
    "sync"
)

// Integer power function
func intPow(base, exp int) int {
	result := 1
	for exp > 0 {
		result *= base
		exp--
	}
	return result
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

        parts := strings.Split(line, " ")

        test_number, err := strconv.Atoi(parts[0][0:len(parts[0])-1])
        if err != nil {
            fmt.Println("Error reading test result number")
            os.Exit(1)
        }
        terms := []int{}
        for i:=1;i<len(parts);i++{
            term_number, err := strconv.Atoi(parts[i])
            if err != nil {
                fmt.Println("Error reading test result number")
                os.Exit(1)
            }
            terms = append(terms, term_number)
        }

        fmt.Printf("Testing %v\n", line)
        
        // Worker function closure that uses `multiplier`
        worker := func(id int, jobs <-chan []bool, results chan<- bool, wg *sync.WaitGroup) {
            defer wg.Done()

            for job := range jobs {
                res := terms[0]
                for i, job_bool := range job {
                    if job_bool {
                        res = res * terms[i+1]
                    } else {
                        res = res + terms[i+1]
                    }
                }
                result := res == test_number
                fmt.Printf("Worker %d processed job: %v -> %v (%v == %v)\n", id, job, result, test_number, res)
                results <- result
            }
        }

        // Inputs and setup
        jobs := make(chan []bool, intPow(2, (len(terms)-1) ))
        results := make(chan bool, intPow(2, (len(terms)-1) ))
        var wg sync.WaitGroup

        // Start workers
        numWorkers := 14
        for i := 1; i <= numWorkers; i++ {
            wg.Add(1)
            go worker(i, jobs, results, &wg)
        }

        // Send jobs
        var inputs [][]bool

        inputs = append(inputs, []bool{true})
        inputs = append(inputs, []bool{false})

        // fmt.Printf("1: %v\n", inputs)
        for i:=1;i<len(terms)-1;i++ {
            new_inputs := [][]bool{}
            // fmt.Printf("2: %v\n", inputs)
            for _, input := range inputs {
                // fmt.Printf("Adding to %v\n", input)
                new_input := []bool{}
                new_input2 := []bool{}

                for _, val := range input {
                    new_input = append(new_input, val)
                    new_input2 = append(new_input2, val)
                }
                new_input = append(new_input, false)
                new_input2 = append(new_input2, true)

                // fmt.Printf("Element 1: %v\n", new_input)
                // fmt.Printf("Element 2: %v\n", new_input2)
                
                new_inputs = append(new_inputs, new_input)
                new_inputs = append(new_inputs, new_input2)
            }
            inputs = new_inputs
            // fmt.Printf("3: %v\n", inputs)
        }


        for _, input := range inputs {
            jobs <- input
        }
        close(jobs)

        // Wait and collect results
        wg.Wait()
        close(results)

        output := false
        for result := range results {
            if result {
                output = true
                break
            }
        }
        fmt.Printf("Final output: %v for %v \n", output, test_number) 

        if output {
            sum += test_number
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
