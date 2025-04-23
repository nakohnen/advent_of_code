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
                        break
                    }
                }
            }
            if found {
                line = line + "#"
                work = append(work[:i], work[i+1:]...)
            } else {
                line = line + "."
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

	quad1 := 0
	quad2 := 0
	quad3 := 0
	quad4 := 0

	height := 103
	width := 101

	for _, r := range robots {
		// func predict(r robot, elapsed int, height, width int) (int, int) {
		x, y := predict(r, 100, height, width)
        fmt.Printf("Robot %v should be here: %d, %d", r, x, y)
		if x < width/2 && y < height/2 {
            fmt.Println(" in quadrant 1")
			quad1 += 1
		} else if x < width/2 && y >= height-height/2 {
            fmt.Println(" in quadrant 2")
			quad2 += 1
		} else if x >= width-width/2 && y < height/2 {
            fmt.Println(" in quadrant 3")
			quad3 += 1
		} else if x >= width-width/2 && y >= height-height/2 {
            fmt.Println(" in quadrant 4")
			quad4 += 1
		} else {
            fmt.Println("")
        }
	}
    fmt.Printf("%d, %d, %d, %d\n", quad1, quad2, quad3, quad4)
	sum = quad1 * quad2 * quad3 * quad4
	fmt.Printf(" -> Sum: %d\n", sum)

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
