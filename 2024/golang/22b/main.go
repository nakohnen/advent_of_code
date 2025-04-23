package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
)

// StringSet type using map[string]struct{}
type StringSet struct {
	m map[string]struct{}
}

// NewStringSet creates a new set
func NewStringSet() *StringSet {
	return &StringSet{m: make(map[string]struct{})}
}

// Add adds an element to the set
func (s *StringSet) Add(value string) {
	s.m[value] = struct{}{}
}

// Contains checks if an element is in the set
func (s *StringSet) Contains(value string) bool {
	_, exists := s.m[value]
	return exists
}

// Remove removes an element from the set
func (s *StringSet) Remove(value string) {
	delete(s.m, value)
}

// Size returns the number of elements in the set
func (s *StringSet) Size() int {
	return len(s.m)
}

// GetElements retrieves all elements in the set
func (s *StringSet) GetElements() []string {
	elements := []string{}
	for p := range s.m {
		elements = append(elements, p)
	}
	return elements
}

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

func getAllSubSequences(n, times int) map[string]int {
	r := n
	last := r
	calSeq := []int{}
    target := make(map[string]int)
	for i := 0; i < times; i++ {
		r = nextSecret(r)
		delta := (r % 10) - (last % 10)
		calSeq = append(calSeq, delta)
		if len(calSeq) >= 5 {
			calSeq = calSeq[1:5]

            sequenceS := []string{}
            for _, j := range calSeq {
                sequenceS = append(sequenceS, fmt.Sprintf("%d", j))
            }
            cmpS := strings.Join(sequenceS, ",")

            if _, exists := target[cmpS]; !exists {
                target[cmpS] = r % 10
            }
		}

        last = r

	}
	return target
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

    worker := func(id int, jobs <-chan int, results chan<- map[string]int, wg *sync.WaitGroup) {
        defer wg.Done()

        for job := range jobs {
            result := getAllSubSequences(job, 2000)
            //fmt.Printf("Worker %d processed job: %d -> %d\n", id, job, result)
            results <- result
        }
    }

    // Inputs and setup
    jobs := make(chan int, len(sequence))
    results := make(chan map[string]int, len(sequence))
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
    
    targetMaps := []map[string]int{}
    candidates := NewStringSet()

    for result := range results {
        targetMaps = append(targetMaps, result)
        for key, _ := range result {
            candidates.Add(key)
        }
    }

    fmt.Printf("%d keys and %d maps\n", candidates.Size(), len(targetMaps))

    for _, cand := range candidates.GetElements() {
        result := 0
        for _, targetMap := range targetMaps {
            result += targetMap[cand]
        }

        if result > sum {
            sum = result
        }
    }

	fmt.Printf(" -> Sum: %d\n", sum)

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
