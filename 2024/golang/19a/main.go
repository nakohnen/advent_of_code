package main

import (
	"bufio"
	"fmt"
	"os"
    "sync"
    "strings"
    "strconv"
)

func readInt(s string) int {
	val, err := strconv.Atoi(s)
	if err != nil {
		//fmt.Printf("Error converting str %s to int\n", s)
		os.Exit(1)
	}
	return val
}

func findCombination(towels []string, design string) bool {
    possible := false

    toWork := []string{design}

    for len(toWork) >  0 {
        current := toWork[0]
        toWork = toWork[1:]

        for _, t := range towels {
            found := true
            if len(t) > len(current) {
                continue
            }
            for i:=0;i<len(t);i++{
                if t[i] != current[i] {
                    found = false
                }
            }

            if found {
                subDesign := current[len(t):]
                //fmt.Printf("%v -> %v with %v\n", current, subDesign, t)
                if len(subDesign) == 0 {
                    return true
                }
                toWork = append(toWork, subDesign)
            }
        }
    }

    return possible
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

    towels := []string{}

    designs := []string{}
	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)
        if len(towels) == 0 {
            towels = strings.Split(line, ", ")
        } else if len(line) > 0 {
            designs = append(designs, line)
        }
	}

    fmt.Printf("Towels %v\n", towels)
    //shorten towels list
    removeIndexes := make(map[int]bool)
    for i, t := range towels {
        //fmt.Printf("%v %v\n", i, t)
        removeIndexes[i] = false
        otherTowels := append([]string(nil), towels[:i]...)
        otherTowels = append(otherTowels, towels[i+1:]...)
        //fmt.Printf("Other towels %v with %v\n", otherTowels, t)
        if findCombination(otherTowels, t) {
            removeIndexes[i] = true
        }
    }
    towels2 := []string{}
    for i, towel := range towels {
        if !removeIndexes[i] {
            towels2 = append(towels2, towel)
        } else {
            fmt.Printf("Removing %s at %d\n", towel, i) 
        }
    }

    fmt.Printf("Towels %v\n", towels2)
    fmt.Printf("Designs %v\n", designs)

    worker := func(id int, jobs <-chan string, results chan<- int, wg *sync.WaitGroup) {
		defer wg.Done()

		for job := range jobs {
            result := 0
                if findCombination(towels2, job) {
                    result = 1
                }
			fmt.Printf("Worker %d processed job: %v -> %d\n", id, job, result)
			results <- result
		}
	}

	// Inputs and setup
	jobs := make(chan string, len(designs))
	results := make(chan int, len(designs))
	var wg sync.WaitGroup

	// Start workers
	numWorkers := 14
	for i := 1; i <= numWorkers; i++ {
		wg.Add(1)
		go worker(i, jobs, results, &wg)
	}

	// Send jobs
	for _, input := range designs {
		jobs <- input
	}
	close(jobs)

	// Wait and collect results
	wg.Wait()
	close(results)

	for result := range results {
        sum += result
	}
   
	fmt.Printf(" -> Sum: %d\n", sum)

    if err4 := scanner.Err(); err4 != nil {
        fmt.Printf("Error reading input file: %v\n", err4)
        os.Exit(1)
    }

	fmt.Println("Processing complete!")
}
