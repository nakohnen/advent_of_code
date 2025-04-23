package main

import (
	"bufio"
	"fmt"
	"os"
    "sync"
)

func contains(t int, slice []int) bool {
	for _, s := range slice {
		if s == t {
			return true
		}
	}
	return false
}

// Define a custom struct
type point struct {
	x int
	y int
}

func checkIfValid(intSlice []int, precedes map[int][]int) (bool, int, int) {
	for i := 1; i < len(intSlice); i++ {
		current := intSlice[i]
		precedesSlice, exists := precedes[current]
		if !exists {
			precedesSlice = []int{}
		}
		fmt.Println(fmt.Sprintf("Checking %v|%v", current, precedesSlice))
		for j, prev := range intSlice[:i] {
			if contains(prev, precedesSlice) {
				return false, i, j
			}
		}

	}
	return true, 0, 0
}

func isPositionALoop(newObstacle point, maxWidth, maxHeight int, guardPosition point, guardDirection rune, obstacleMap map[point]bool) bool {

	left := false
    loop := false
    
    var positionHistory []point
    var directionHistory []rune


	for !left && !loop {
        // fmt.Println(fmt.Sprintf("Current position and direction %v %s", guardPosition, string(guardDirection) ))

		nextPosition := guardPosition
		nextDirection := guardDirection
		if guardDirection == 'U' {
			nextPosition.y += -1
			nextDirection = 'R'
		} else if guardDirection == 'R' {
			nextPosition.x += 1
			nextDirection = 'D'
		} else if guardDirection == 'D' {
			nextPosition.y += 1
			nextDirection = 'L'
		} else {
			nextPosition.x += -1
			nextDirection = 'U'
		}
		if nextPosition.x < 0 || nextPosition.x > maxWidth || nextPosition.y < 0 ||
			nextPosition.y > maxHeight {
			left = true
			break
		}

		if obstacleMap[nextPosition] || nextPosition == newObstacle {
			guardDirection = nextDirection
		} else {
			guardPosition = nextPosition
		}

        for i, pos := range positionHistory {
            if pos == guardPosition {
                if directionHistory[i] == guardDirection {
                    loop = true
                    break
                }
            }
        }
        
        positionHistory = append(positionHistory, guardPosition)
        directionHistory = append(directionHistory, guardDirection)

	}
    return loop
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

	obstacleMap := make(map[point]bool)
	var guardPosition point
	guardDirection := rune('U')

	y := 0

	maxHeight := 0
	maxWidth := 0

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

		for x, char := range line {
			p := point{x, y}
			obstacleMap[p] = false
			if char == '#' {
				obstacleMap[p] = true
			}
            if char == '^' {
				guardPosition = p
			}

			if x > maxWidth {
				maxWidth = x
			}
		}
		if y > maxHeight {
			maxHeight = y
		}

        y += 1
	}

    fmt.Println(fmt.Sprintf("maxWidth maxHeight %v %v", maxWidth, maxHeight))

	// Worker function closure that uses `multiplier`
	worker := func(id int, jobs <-chan point, results chan<- int, wg *sync.WaitGroup) {
		defer wg.Done()

		for job := range jobs {
            result := 0
            if isPositionALoop(job, maxWidth, maxHeight, guardPosition, guardDirection, obstacleMap) {
                result = 1
            }

			fmt.Printf("Worker %d processed job: %d -> %d\n", id, job, result)
			results <- result
		}
	}

	// Inputs and setup
	jobs := make(chan point, (maxWidth + 1) * (maxHeight + 1))
	results := make(chan int, (maxWidth + 1) * (maxHeight + 1))
	var wg sync.WaitGroup

	// Start workers
	numWorkers := 14 * 4
	for i := 1; i <= numWorkers; i++ {
		wg.Add(1)
		go worker(i, jobs, results, &wg)
	}

	// Send jobs
    for x := 0; x <= maxWidth; x++ {
        for y := 0; y <= maxHeight; y++ {
            jobs <- point{x, y}
        }
    }
	close(jobs)

	// Wait and collect results
	wg.Wait()
	close(results)

	for result := range results {
        sum += result
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
