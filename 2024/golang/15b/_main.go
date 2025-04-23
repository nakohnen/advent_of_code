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

func move(tilemap map[point]int, robotPosition point, direction rune) point {
	newPosition := point{robotPosition.x, robotPosition.y}
	newPositionL := point{robotPosition.x - 1, robotPosition.y}
	newPositionR := point{robotPosition.x + 1, robotPosition.y}
	var positionOther point
	flagHorizontal := false
	switch direction {
	case 'U':
		newPosition.y -= 1
		newPositionL.y -= 1
		newPositionR.y -= 1
	case 'D':
		newPosition.y += 1
		newPositionL.y += 1
		newPositionR.y += 1
	case 'L':
		newPosition.x -= 1
		flagHorizontal = true
	case 'R':
		newPosition.x += 1
		flagHorizontal = true
	}
	targetPosition := point{newPosition.x, newPosition.y}

	old := point{newPosition.x, newPosition.y}
	var oldOther point

	if !flagHorizontal {
		if tilemap[newPosition] == 2 {
			oldOther = newPositionR
			positionOther = newPositionR
		} else {
			oldOther = newPositionL
			positionOther = newPositionL
		}
	}

	isFree := false
	pushed := false

	for !isFree {
		if flagHorizontal {
			tile := tilemap[newPosition]
			fmt.Printf("%v %s %v\n", newPosition, string(direction), tile)
			if tile == 0 {
				isFree = true
			} else if tile == 1 {
				return robotPosition
			} else if tile == 2 || tile == 3 {
				switch direction {
				case 'L':
					newPosition.x -= 1
				case 'R':
					newPosition.x += 1
                default:
                    os.Exit(1)
				}
				pushed = true
			}
		} else {
			tile := tilemap[newPosition]
			tileOther := tilemap[positionOther]
			//fmt.Printf("%s %v=%v (other %v=%v)\n", string(direction), tile, newPosition, tileOther, positionOther)

            if tile == 1  {
                return robotPosition
            } else if tile == 0 && tileOther == 0 {
				isFree = true
            } else if tile == 0 && !pushed {
                isFree = true
            } else if tile == 0 && pushed {
                return robotPosition
			} else if pushed && tile != tilemap[old] && tileOther != tilemap[oldOther] {
				return robotPosition
			} else if (!pushed && (tile == 2 || tile == 3)) || (pushed && tile == tilemap[old] && tileOther == tilemap[oldOther]) {
				old = point{newPosition.x, newPosition.y}
				oldOther = point{positionOther.x, positionOther.y}
				switch direction {
				case 'U':
					newPosition.y -= 1
					positionOther.y -= 1
				case 'D':
					newPosition.y += 1
					positionOther.y += 1
				default:
					os.Exit(1)
				}
				pushed = true
			}
		}
	}
	if pushed {
		if flagHorizontal {
			lower := Min(targetPosition.x, newPosition.x)
			higher := Max(targetPosition.x+1, newPosition.x+1)
			flip := false
            if direction == 'L' {
                flip = !flip
            }
			for x := lower; x < higher; x++ {
				newP := point{x, targetPosition.y}
				if flip {
					tilemap[newP] = 2
				} else {
					tilemap[newP] = 3
				}
				flip = !flip
			}
			tilemap[targetPosition] = 0
		} else {
			lower := Min(targetPosition.y, newPosition.y)
			higher := Max(targetPosition.y+1, newPosition.y+1)

            xLower := Min(newPosition.x, positionOther.x)
            xHigher := Max(newPosition.x, positionOther.x)

			for y := lower; y < higher; y++ {
				newP := point{xLower, y}
                newP2 := point{xHigher, y}
                tilemap[newP] = 2
                tilemap[newP2] = 3
			}
			tilemap[point{newPosition.x, targetPosition.y}] = 0
            tilemap[point{positionOther.x, targetPosition.y}] = 0
        }
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
