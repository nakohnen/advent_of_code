package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
)

func readInt(s string) int {
	val, err := strconv.Atoi(s)
	if err != nil {
		//fmt.Printf("Error converting str %s to int\n", s)
		os.Exit(1)
	}
	return val
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func mix(a, b int) int {
	return a ^ b
}

func prune(n int) int {
	return n % 16777216
}

func nextSecret(n int) int {
	step1 := prune(mix(n*64, n))
	step2 := prune(mix(step1/32, step1))
	step3 := prune(mix(step2*2048, step2))
	return step3
}

func simulate(n, times int, sequence []int) int {
	r := n
	sequenceS := []string{}
	for _, i := range sequence {
		sequenceS = append(sequenceS, fmt.Sprintf("%d", i))
	}
	cmp := strings.Join(sequenceS, ",")
	//fmt.Printf("To see: %s\n", cmp)
	last := r
	calSeq := []int{}
	for i := 0; i < times; i++ {
		r = nextSecret(r)
		delta := (r % 10) - (last % 10)
		calSeq = append(calSeq, delta)
		if len(calSeq) >= 5 {
			calSeq = calSeq[1:5]
		}

		sequenceS = []string{}
		for _, j := range calSeq {
			sequenceS = append(sequenceS, fmt.Sprintf("%d", j))
		}
		cmpS := strings.Join(sequenceS, ",")

		//fmt.Printf("New: %d => %s with delta (%d)\n", r, cmpS, delta)
		if cmpS == cmp {
			//fmt.Printf("Code %d after %d mutations\n", r, i)
			return r % 10
		}
		last = r
	}
	return 0
}

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

	sequence := []int{}

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)
		n := readInt(line)
		//r := simulate(n, 2000)
		sequence = append(sequence, n)
		//fmt.Printf("%d -> %d\n", n, r)
		//sum += r
	}
	for i1 := -9; i1 <= 9; i1++ {
		for i2 := -9; i2 <= 9; i2++ {
			for i3 := -9; i3 <= 9; i3++ {
				for i4 := -9; i4 <= 9; i4++ {
					toSee := []int{i1, i2, i3, i4}
					// Worker function closure that uses `multiplier`
					worker := func(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
						defer wg.Done()

						for job := range jobs {
							result := simulate(job, 2000, toSee)
							//fmt.Printf("Worker %d processed job: %d -> %d\n", id, job, result)
							results <- result
						}
					}

					// Inputs and setup
					jobs := make(chan int, len(sequence))
					results := make(chan int, len(sequence))
					var wg sync.WaitGroup

					// Start workers
					numWorkers := 14
					if numWorkers > len(sequence) {
						numWorkers = len(sequence)
					}

					for i := 1; i <= numWorkers; i++ {
						wg.Add(1)
						go worker(i, jobs, results, &wg)
					}

					// Send jobs
					for _, input := range sequence {
						jobs <- input
					}
					close(jobs)

					// Wait and collect results
					wg.Wait()
					close(results)
					subresult := 0
					for result := range results {
						subresult += result
					}
					fmt.Printf("%v => %v\n", toSee, subresult)
					if subresult > sum {
						sum = subresult
					}
				}
			}
		}
	}

	fmt.Printf(" -> Sum: %d\n", sum)

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
