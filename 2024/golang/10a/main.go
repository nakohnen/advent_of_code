package main

import (
	"bufio"
	"fmt"
	"os"
	"sync"
	//"strings"
	"strconv"
)

type point struct {
	x, y int
}

func appendIfNew[T comparable](slice []T, element T) ([]T, bool) {
	for _, o := range slice {
		if o == element {
			return slice, false
		}
	}
	slice = append(slice, element)
	return slice, true
}

func walkMap(start point, topoMap [][]int, adjTopoMap map[point][]point, endValue int) []point {
	result := []point{}

	candidates := adjTopoMap[start]

	for len(candidates) > 0 {
		nextPos := candidates[len(candidates)-1]
		candidates = candidates[:len(candidates)-1]

		if topoMap[nextPos.y][nextPos.x] == endValue {
			result, _ = appendIfNew(result, nextPos)
			continue
		}

		for _, newCandidate := range adjTopoMap[nextPos] {
			candidates = append(candidates, newCandidate)
		}

	}
	return result
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

	maxWidth := 0
	maxHeight := 0

	var topoMap [][]int

	{
		y := 0
		for scanner.Scan() {
			line := scanner.Text()
			//fmt.Println(line)
			var localMap []int
			for x, sVal := range line {
				iVal, err := strconv.Atoi(string(sVal))
				if err != nil {
					fmt.Printf("Error converting value at (%v, %v): %v\n", x, y, string(sVal))
				}
				if x >= maxWidth {
					maxWidth = x + 1
				}
				localMap = append(localMap, iVal)
			}

			if y >= maxHeight {
				maxHeight = y + 1
			}
			topoMap = append(topoMap, localMap)
			y += 1
		}
		fmt.Printf("maxWidth: %v; maxHeight: %v\n", maxWidth, maxHeight)
	}

	isInside := func(p point) bool {
		return p.x >= 0 && p.y >= 0 && p.x < maxWidth && p.y < maxHeight

	}

	adjTopoMap := make(map[point][]point) // From point to surrounding points
	for x := 0; x < maxWidth; x++ {
		for y := 0; y < maxHeight; y++ {
			p := point{x, y}
			targetPoints := []point{}
			targetPoints = append(targetPoints, point{x - 1, y})
			targetPoints = append(targetPoints, point{x + 1, y})
			targetPoints = append(targetPoints, point{x, y - 1})
			targetPoints = append(targetPoints, point{x, y + 1})

			adjTopoMap[p] = []point{}
			for _, pOther := range targetPoints {
				if isInside(pOther) && topoMap[pOther.y][pOther.x]-topoMap[p.y][p.x] == 1 {
					adjTopoMap[p] = append(adjTopoMap[p], pOther)
				}
			}
		}
	}

	// Worker function closure that uses `multiplier`
	worker := func(id int, jobs <-chan point, results chan<- int, wg *sync.WaitGroup) {
		defer wg.Done()

		for job := range jobs {

			// func walkMap(start point, topoMap [][]int, adjTopoMap map[point][]point, endValue int) []point {
			result := walkMap(job, topoMap, adjTopoMap, 9)
			fmt.Printf("Worker %d processed job: %d -> %d\n", id, job, len(result))
			results <- len(result)
		}
	}

	var inputs []point
	for x := 0; x < maxWidth; x++ {
		for y := 0; y < maxHeight; y++ {
            if topoMap[y][x] == 0 {
                inputs = append(inputs, point{x, y})
            }
		}
	}

	// Inputs and setup
	jobs := make(chan point, len(inputs))
	results := make(chan int, len(inputs))
	var wg sync.WaitGroup

	// Start workers
	numWorkers := 14
	for i := 1; i <= numWorkers; i++ {
		wg.Add(1)
		go worker(i, jobs, results, &wg)
	}

	// Send jobs
	for _, input := range inputs {
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
