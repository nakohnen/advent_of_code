package main

import (
	"bufio"
	"fmt"
	"os"
	//"sync"
	"strings"
	"strconv"
)

type robot struct {
	x, y, vx, vy int
}

type position struct {
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
func predict(r robot, elapsed int, height, width int) (int, int) {
	pX := (r.x + r.vx*elapsed) % width
	pY := (r.y + r.vy*elapsed) % height
    if pX < 0 {
        pX += width
    }
    if pY < 0 {
        pY += height
    }
	return pX, pY
}

func atoi(input string) int {
	val, err := strconv.Atoi(input)
	if err != nil {
		fmt.Printf("Error converting %s\n", input)
		os.Exit(1)
	}
	return val
}

func print(positions []position, width, height int) {
    work := append([]position(nil), positions...)
    for y:=0; y<height;y++{
        line := ""
        for x:=0; x<width;x++ {
            foundI := -1
            found := false
            if len(work) > 0 {
                for i, p := range work {
                    if p.x == x && p.y == y {
                        found = true
                        foundI = i
                        break
                    }
                }
            }
            if found {
                line = line + "#"
                work = append(work[:foundI], work[foundI+1:]...)
            } else {
                line = line + " "
            }
        }
        fmt.Println(line)
    }
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

	robots := []robot{}

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

		parts := strings.Split(line, " ")
        if len(parts) != 2 {
            fmt.Println(line)
            continue
        }
		position := parts[0][2:]
		velocity := parts[1][2:]
		position2 := strings.Split(position, ",")
		velocity2 := strings.Split(velocity, ",")

		r := robot{
			atoi(position2[0]),
			atoi(position2[1]),
			atoi(velocity2[0]),
			atoi(velocity2[1]),
		}
		robots = append(robots, r)

	}

	height := 103
	width := 101

    nextV := 14
    nextH := 76
    for i:=1;i<100000000;i++ {
        if i != 7286 {
            continue
        }
        if i == nextV {
            nextV += 101
        }
        if i == nextH {
            nextH += 103
        }
        positions := []position{}
        for _, r := range robots {
            // func predict(r robot, elapsed int, height, width int) (int, int) {
            x, y := predict(r, i, height, width)
            var appended bool
            positions, appended = appendIfNew(positions, position{x, y})
            if !appended {
                continue
            }
        }
        fmt.Printf("%d seconds -------------------------------------------\n", i)
        print(positions, height, width)
        fmt.Println("")
        break
    }
	fmt.Printf(" -> Sum: %d\n", sum)

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
