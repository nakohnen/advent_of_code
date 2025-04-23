package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
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

func isPossible(towels []string, design string) bool {
	possible := false

	toWork := []string{design}

	for len(toWork) > 0 {
		current := toWork[0]
		toWork = toWork[1:]

		for _, t := range towels {
			found := true
			if len(t) > len(current) {
				continue
			}
			for i := 0; i < len(t); i++ {
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


func bruteForce(towels []string, design string) int {
	toWork := []string{design}
    toWorkComp := [][]string{[]string{}}

    done := [][]string{}

	for len(toWork) > 0 {
		current := toWork[0]
		toWork = toWork[1:]
        comp := toWorkComp[0]
        toWorkComp = toWorkComp[1:]

		for _, t := range towels {
			found := true
			if len(t) > len(current) {
				continue
			}
			for i := 0; i < len(t); i++ {
				if t[i] != current[i] {
					found = false
				}
			}

			if found {
				subDesign := current[len(t):]
                newComp := append([]string(nil), comp...)
                newComp = append(newComp, t)
				//fmt.Printf("%v -> %v with %v\n", current, subDesign, t)
				if len(subDesign) == 0 {
                    done = append(done, newComp)
				} else {
                    toWork = append(toWork, subDesign)
                    toWorkComp = append(toWorkComp, newComp)
                }
			}
		}
	}
    done = removeDuplicates(done)
    for _, comp := range done {
        fmt.Printf("Comp %v = %s\n", comp, strings.Join(comp,""))
    }
	return len(done)
}

func removeDuplicates(slice [][]string) [][]string {
	result := [][]string{}
	sliceSimple := []string{}
	ignore := make(map[int]bool)
	for i, sub := range slice {
		ignore[i] = false
		comb1 := strings.Join(sub, "_")
		sliceSimple = append(sliceSimple, comb1)
	}

	for i, sub := range sliceSimple {
		for j := i + 1; j < len(sliceSimple); j++ {
			if !ignore[j] && sub == sliceSimple[j] {
				ignore[j] = true
			}
		}
	}

	for i, sub := range slice {
		if !ignore[i] {
			result = append(result, sub)
		}
	}
	return result
}

func contains(slice [][]string, other []string) bool {
	for _, sub := range slice {
		same := true
		if len(sub) != len(other) {
			continue
		}
		for i := 0; i < len(other); i++ {
			if other[i] != sub[i] {
				same = false
			}
		}
		if same {
			return true
		}
	}
	return false
}

func findCombination(towels []string, design string, removed []string) int {
	fullLength := len(design)
	fmt.Printf("Length of design: %d\n", fullLength)
	toWork := []string{design}
	var toWorkComp [][]string
	toWorkComp = append(toWorkComp, []string{})

	done := [][]string{}
	for len(toWork) > 0 {
		current := toWork[0]
		toWork = toWork[1:]

		comp := toWorkComp[0]
		toWorkComp = toWorkComp[1:]

		for _, t := range towels {
			found := true
			if len(t) > len(current) {
				continue
			}
			for i := 0; i < len(t); i++ {
				if t[i] != current[i] {
					found = false
				}
			}

			if found {
				subDesign := current[len(t):]
				newComp := append([]string(nil), comp...)
				newComp = append(newComp, t)
				if len(subDesign) == 0 && strings.Join(newComp, "") == design {
					fmt.Printf("%v -> %v (len %d) with %v as %v\n", current, subDesign, len(subDesign), t, newComp)
					done = append(done, newComp)
					//fmt.Println("appended")
				} else {
					toWork = append(toWork, subDesign)
					toWorkComp = append(toWorkComp, newComp)
					//fmt.Println("Continued")
				}
			}
		}
	}

	done = removeDuplicates(done)
	fmt.Printf("Design %v can be done using %v base towels\n", design, len(done))
	for _, comp := range done {
		fmt.Printf("using %v\n", comp)
	}

	result := 0

	for _, comp := range done {

		removed2 := []string{}
		for _, rt := range removed {
			if isPossible(comp, rt) {
				removed2 = append(removed2, rt)
			}
		}

		//fmt.Printf("Reintroduced: %v\n", removed2)

		combinedPositions := [][]int{}

		for i := 0; i < len(comp); i++ {
			for j := i; j < len(comp); j++ {
				combined := strings.Join(comp[i:j+1], "")

				for _, rt := range removed2 {
					if combined == rt {
						//fmt.Println(combined)
						combinedPositions = append(combinedPositions, []int{i, j})
					}
				}
			}
		}
		fmt.Printf("Combined Positions: %v\n", combinedPositions)

	}
	return result
}

func find(allTowels []string, design string, memo map[string]int) int {
	if result, exists := memo[design]; exists {
		return result
	}

	lenDesign := len(design)
	if lenDesign <= 2 {
		result := 0
		switch lenDesign {
		case 1:
			for _, t := range allTowels {
				if t == design {
					result = 1
				}
			}
		case 2:
			for _, t := range allTowels {
				if t == design {
					result += 1
				}
			}
			foundBoth := true
			for _, char := range design {
				found := false
				for _, t := range allTowels {
					if string(char) == t {
						found = true
						break
					}
				}
				if !found {
					foundBoth = false
					break
				}
			}
			if foundBoth {
				result += 1
			}
		case 0:
			result = 0
		}
		memo[design] = result
		return result
	}
	part1 := find(allTowels, string(design[0]), memo)
	part2 := find(allTowels, design[1:], memo)
	part3 := find(allTowels, design[:1], memo)
	part4 := find(allTowels, design[2:], memo)

	result := part1*part2 + part3*part4
	memo[design] = result
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

	towels := []string{}

	designsOld := []string{}
	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)
		if len(towels) == 0 {
			towels = strings.Split(line, ", ")
		} else if len(line) > 0 {
			designsOld = append(designsOld, line)
		}
	}
	designs := designsOld
	//designs := designsOld[:1]

	// Sort by length and then alphanumerically
	sort.Slice(towels, func(i, j int) bool {
		return towels[i] < towels[j] // Alphanumerical for equal lengths
	})

	fmt.Printf("Towels %v\n", towels)
	//shorten towels list
	removeIndexes := make(map[int]bool)
	for i, t := range towels {
		//fmt.Printf("%v %v\n", i, t)
		removeIndexes[i] = false
		otherTowels := append([]string(nil), towels[:i]...)
		otherTowels = append(otherTowels, towels[i+1:]...)
		//fmt.Printf("Other towels %v with %v\n", otherTowels, t)
		if isPossible(otherTowels, t) {
			removeIndexes[i] = true
		}
	}
	towels2 := []string{}
	removed := []string{}
	for i, towel := range towels {
		if !removeIndexes[i] {
			towels2 = append(towels2, towel)
		} else {
			removed = append(removed, towel)
			//fmt.Printf("Removing %s at %d\n", towel, i)
		}
	}

	fmt.Printf("Towels %v\n", towels2)
	//fmt.Printf("Designs %v\n", designs)

	worker := func(id int, jobs <-chan string, results chan<- int, wg *sync.WaitGroup) {
		defer wg.Done()

		for job := range jobs {
			result := bruteForce(towels, job)
			/**if isPossible(towels2, job) {
				fmt.Printf("Design %v is possible\n", job)
				result = findCombination(towels2, job, removed)
			}**/
			fmt.Printf("Worker %d processed job: %v -> %d\n", id, job, result)
			fmt.Println("")
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
