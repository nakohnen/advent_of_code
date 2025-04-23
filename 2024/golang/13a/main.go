package main

import (
	"bufio"
	"fmt"
	"os"
    "sync"
    "strings"
    "strconv"
)

type machine struct {
    xA, yA, xB, yB, xP, yP int
}

type winningPress struct {
    a, b int
}

func calculateCheapestCost(m machine, aCost, bCost, maxPresses int) int {
    xA := m.xA
    xB := m.xB
    xP := m.xP
    yA := m.yA
    yB := m.yB
    yP := m.yP

    results := []winningPress{}
    
    for a := 0; a <= maxPresses; a++ {
        for b := 0; b <= maxPresses; b++ {
            deltaA := xP - (a * xA + b * xB)
            deltaB := yP - (a * yA + b * yB)
            if deltaA == 0 && deltaB == 0 {
                results = append(results, winningPress{a,b})
            } else if deltaA < 0 || deltaB < 0 {
                break
            }
        }
        if a * xA > xP || a * yA > yP {
            break
        }
    }

    if len(results) == 0 {
        return 0
    }

    fmt.Printf("%v solutions %d\n", m, len(results))

    cheapest := aCost * maxPresses + bCost * maxPresses 
    for _, win := range results {
        fmt.Printf("%v\n", win)
        res := aCost * win.a + bCost * win.b 
        if res < cheapest {
            cheapest = res
        }
    }
    return cheapest
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

    machines := []machine{}

    currentMachine := machine{0,0,0,0,0,0}

    buttonACost := 3
    buttonBCost := 1

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)
        
        if len(line) > 6 {
            parts := strings.Split(line, " ")
            if parts[0] == "Button" {
                if parts[1][0] == 'A' {
                    xStr := parts[2][2:len(parts[2])-1]

                    yStr := parts[3][2:]

                    x, err := strconv.Atoi(xStr)
                    if err != nil {
                        os.Exit(1)
                    }

                    y, err := strconv.Atoi(yStr)
                    if err != nil {
                        os.Exit(1)
                    }


                    currentMachine.xA = x
                    currentMachine.yA = y

                } else if parts[1][0] == 'B' {
                    xStr := parts[2][2:len(parts[2])-1]

                    yStr := parts[3][2:]

                    x, err := strconv.Atoi(xStr)
                    if err != nil {
                        os.Exit(1)
                    }

                    y, err := strconv.Atoi(yStr)
                    if err != nil {
                        os.Exit(1)
                    }


                    currentMachine.xB = x
                    currentMachine.yB = y

                }
            } else if parts[0] == "Prize:" {
                xStr := parts[1][2:len(parts[1])-1]

                yStr := parts[2][2:]

                x, err := strconv.Atoi(xStr)
                if err != nil {
                    os.Exit(1)
                }

                y, err := strconv.Atoi(yStr)
                if err != nil {
                    os.Exit(1)
                }

                currentMachine.xP = x
                currentMachine.yP = y

                machines = append(machines, currentMachine)
                fmt.Printf("Machine %v added.\n", currentMachine)
                currentMachine = machine{0,0,0,0,0,0}
            }
        }
	}

    fmt.Printf("We have %d machines\n", len(machines))

    worker := func(id int, jobs <-chan machine, results chan<- int, wg *sync.WaitGroup) {
        defer wg.Done()

        for job := range jobs {

//func calculateCheapestCost(m machine, aCost, bCost, maxPresses int) int {
            result := calculateCheapestCost(job, buttonACost, buttonBCost, 100)
            fmt.Printf("Worker %d processed job: %d -> %d\n", id, job, result)
            results <- result
        }
    }

    // Inputs and setup
    jobs := make(chan machine, len(machines))
    results := make(chan int, len(machines))
    var wg sync.WaitGroup

    // Start workers
    numWorkers := 1
    for i := 1; i <= numWorkers; i++ {
        wg.Add(1)
        go worker(i, jobs, results, &wg)
    }

    // Send jobs
    for _, input := range machines {
        jobs <- input
    }
    close(jobs)

    // Wait and collect results
    wg.Wait()
    close(results)

    output := []int{}
    for result := range results {
        output = append(output, result)
    }

    for _, c := range output {
        sum += c
    }
    fmt.Printf(" -> Sum: %d\n", sum)

    if err4 := scanner.Err(); err4 != nil {
        fmt.Printf("Error reading input file: %v\n", err4)
        os.Exit(1)
    }

	fmt.Println("Processing complete!")
}
