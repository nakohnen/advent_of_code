package main

import (
	"bufio"
	"fmt"
	"os"
    //"sync"
    //"strings"
    //"strconv"
)

type point struct {
    x, y int
}

func move(tilemap map[point]int, robotPosition point, direction rune) point {
    newPosition := point{robotPosition.x, robotPosition.y}
    switch direction {
        case 'U':
            newPosition.y -= 1
        case 'D':
            newPosition.y += 1
        case 'L':
            newPosition.x -= 1
        case 'R':
            newPosition.x += 1
    }
    targetPosition := point{newPosition.x, newPosition.y}
    isFree := false
    pushed := false
    for !isFree {
        tile := tilemap[newPosition]
        fmt.Printf("%v %s %v\n", newPosition, string(direction), tile)
        switch tile {
        case 0:
            isFree = true
        case 1:
            return robotPosition
        case 2:
            switch direction {
            case 'U':
                newPosition.y -= 1
            case 'D':
                newPosition.y += 1
            case 'L':
                newPosition.x -= 1
            case 'R':
                newPosition.x += 1
            }
            pushed = true
        default:
            os.Exit(1)
        }
    }
    if pushed {
        tilemap[targetPosition] = 0
        tilemap[newPosition] = 2
    }
    return targetPosition
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

    tilemap := make(map[point]int)
    movements := []rune{}

    maxWidth := 0
    maxHeight := 0

    robotPosition := point{-1,-1}

    { 
        y:= 0
        mapMode := true

        for scanner.Scan() {
            line := scanner.Text()
            //fmt.Println(line)

            if len(line) < 2 {
                mapMode = false
            }
            if len(line) > 0 {
                if mapMode {
                    for x, char := range line {
                        p := point{x, y}
                        tilemap[p] = 0
                        if x > maxWidth {
                            maxWidth = x + 1
                        }
                        r := rune(char)
                        if r == '#' {
                            tilemap[p] = 1
                        } else if r == 'O' {
                            tilemap[p] = 2
                        } else if r == '@' {
                            tilemap[p] = 0
                            robotPosition = p
                        }
                    }
                    if y > maxHeight {
                        maxHeight = y +1
                    }
                    y += 1
                } else {
                    for _, char := range line {
                        r := rune(char)
                        switch r {
                        case '<':
                            movements = append(movements, 'L')
                        case '>':
                            movements = append(movements, 'R')
                        case '^':
                            movements = append(movements, 'U')
                        case 'v':
                            movements = append(movements, 'D')
                        }
                    }
                }
            }
        }
    }

    fmt.Printf("%d movements to go.\n", len(movements))

    for _, direction := range movements {
        fmt.Printf("Moving %v %v\n", direction, robotPosition)
        newPosition := move(tilemap, robotPosition, direction)
        fmt.Printf("Moving %v %v to %v\n", direction, robotPosition, newPosition)
        robotPosition = newPosition
    }

    for x:=0;x<maxWidth;x++{
        for y:=0;y<maxHeight;y++{
            p := point{x, y}
            if tilemap[p] == 2 {
                sum += x + 100 * y
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
