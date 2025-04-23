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

// Max returns the larger of two integers
func Max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// Min returns the smaller of two integers
func Min(a, b int) int {
	if a < b {
		return a
	}
	return b
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

func printMap(tilemap map[point]int, maxWidth, maxHeight int, robotPosition point) {
	for y := 0; y < maxHeight; y++ {
		line := ""
		for x := 0; x < maxWidth; x++ {
			p := point{x, y}
			if p == robotPosition {
				line = line + "@"
			} else {
				switch t := tilemap[p]; t {
				case 0:
					line = line + " "
				case 1:
					line = line + "#"
				case 2:
					line = line + "["
				case 3:
					line = line + "]"
				default:
					os.Exit(1)
				}
			}
		}
		fmt.Println(line)
	}
}

func canMove(tilemap map[point]int, position point, direction point) ([]point, bool) {
    target := point{position.x + direction.x, position.y + direction.y}
    targetTile := tilemap[target]
    if targetTile == 0 {
        return []point{position}, true
    } else if targetTile == 1 {
        return []point{}, false
    } else {
        other, move := canMove(tilemap, target, direction)
        if direction.y != 0 {
            xAdd := -1
            if targetTile == 2 {
                xAdd = 1
            }
            target2 := point{target.x + xAdd, target.y}
            o2, move2 := canMove(tilemap, target2, direction)
            other = append(other, o2...)
            move = move && move2
        }
        other = append(other, position)
        return other, move  
    }
}

func move(tilemap map[point]int, robotPosition point, direction rune) point {
    directionVector := point{0, 0}
    if direction == 'U' {
        directionVector.y = -1
    } else if direction == 'D' {
        directionVector.y = +1
    } else if direction == 'R' {
        directionVector.x = +1
    } else if direction == 'L' {
        directionVector.x = -1
    } else {
        os.Exit(1)
    }

    movePointsT, valid := canMove(tilemap, robotPosition, directionVector)
    if valid {
        movePoints := []point{}
        for _, p := range movePointsT {
            movePoints, _ = appendIfNew(movePoints, p)
        }
        oldData := make(map[point]int)
        for _, p := range movePoints {
            oldData[p] = tilemap[p]
            tilemap[p] = 0
        }
        dv := directionVector
        for _, p := range movePoints {
            target := point{p.x + dv.x, p.y + dv.y}
            tilemap[target] = oldData[p]
        }
        robotPosition.x += dv.x
        robotPosition.y += dv.y
    }
    return robotPosition
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

	robotPosition := point{-1, -1}

	{
		y := 0
		mapMode := true

		for scanner.Scan() {
			line := scanner.Text()
			//fmt.Println(line)

			if len(line) < 2 {
				mapMode = false
			}
			if len(line) > 0 {
				if mapMode {
					for xp, char := range line {
						x := xp * 2
						p := point{x, y}
						p2 := point{x + 1, y}
						tilemap[p] = 0
						if x > maxWidth {
							maxWidth = x + 2
						}
						r := rune(char)
						if r == '#' {
							tilemap[p] = 1
							tilemap[p2] = 1
						} else if r == 'O' {
							tilemap[p] = 2
							tilemap[p2] = 3
						} else if r == '@' {
							tilemap[p] = 0
							tilemap[p2] = 0
							robotPosition = p
						}
					}
					if y > maxHeight {
						maxHeight = y + 1
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

	fmt.Println("Starting position:")
	printMap(tilemap, maxWidth, maxHeight, robotPosition)
    //var input string
	for _, direction := range movements {
		fmt.Printf("Moving %v %v\n", string(direction), robotPosition)
		newPosition := move(tilemap, robotPosition, direction)
        fmt.Printf("Moving %v %v to %v\n", string(direction), robotPosition, newPosition)
		robotPosition = newPosition
		printMap(tilemap, maxWidth, maxHeight, robotPosition)
		fmt.Println("")
        //fmt.Scanln(&input)
    }

	for x := 0; x < maxWidth; x++ {
		for y := 0; y < maxHeight; y++ {
			p := point{x, y}
			if tilemap[p] == 2 {
				sum += x + 100*y
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
