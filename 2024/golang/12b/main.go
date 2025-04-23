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

func appendIfNew[T comparable](slice []T, element T) ([]T, bool) {
	for _, other := range slice {
		if other == element {
			return slice, false
		}
	}
	return append(slice, element), true
}

func remove[T comparable](slice []T, element T) ([]T, bool) {
	for i, other := range slice {
		if other == element {
			return append(slice[:i], slice[i+1:]...), true
		}
	}
	return slice, false
}

func contains[T comparable](slice []T, element T) bool {
	for _, other := range slice {
		if other == element {
			return true
		}
	}
	return false
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

	plotMap := make(map[point]rune)
	regionMap := make(map[point]int)
	regionPlots := make(map[int][]point)
	neighboursPlot := make(map[point][]point)

	maxWidth := 0
	maxHeight := 0
	{
		y := 0
		for scanner.Scan() {
			line := scanner.Text()
			//fmt.Println(line)

			for x, char := range line {
				pos := point{x, y}

				plotMap[pos] = rune(char)

				if x >= maxWidth {
					maxWidth = x + 1
				}
			}

			if y >= maxHeight {
				maxHeight = y + 1
			}

			y += 1
		}
	}

	visited := make(map[point]bool)
	for x := 0; x < maxWidth; x++ {
		for y := 0; y < maxHeight; y++ {
			pos := point{x, y}
			visited[pos] = false
			neighboursPlot[pos] = []point{}
			regionMap[pos] = y*maxWidth + x

			candidates := []point{}
			candidates = append(candidates, point{x - 1, y})
			candidates = append(candidates, point{x + 1, y})
			candidates = append(candidates, point{x, y - 1})
			candidates = append(candidates, point{x, y + 1})

			for len(candidates) > 0 {
				cand := candidates[0]
				candidates = candidates[1:]

				if cand.x < 0 || cand.x >= maxWidth {
					continue
				}
				if cand.y < 0 || cand.y >= maxHeight {
					continue
				}

				if plotMap[pos] == plotMap[cand] {
					neighboursPlot[pos], _ = appendIfNew(neighboursPlot[pos], cand)
				}
			}
		}
	}

	for x := 0; x < maxWidth; x++ {
		for y := 0; y < maxHeight; y++ {
			pos := point{x, y}

			if !visited[pos] {
				fmt.Printf("Visiting %v (%s)\n", pos, string(plotMap[pos]))
				visited[pos] = true

				currentRegion := regionMap[pos]
				regionPlots[currentRegion] = []point{pos}

				candidates := append([]point(nil), neighboursPlot[pos]...)
				newStartingPositions := []point{}
				cPos := pos

				for len(candidates) > 0 {
					cand := candidates[0]
					candidates = candidates[1:]

					if !visited[cand] {
						newStartingPositions, _ = appendIfNew(newStartingPositions, cand)
					} else {
						regionMap[cand] = regionMap[cPos]
						regionPlots[currentRegion], _ = appendIfNew(regionPlots[currentRegion], cPos)
					}

					if len(candidates) == 0 && len(newStartingPositions) > 0 {
						cPos = newStartingPositions[0]
						newStartingPositions = newStartingPositions[1:]
						candidates = append([]point(nil), neighboursPlot[cPos]...)
						visited[cPos] = true
					}
				}
			}
		}
	}

	for region, plots := range regionPlots {
		size := 0
		xLower := maxWidth
		xHigher := 0
		yLower := maxHeight
		yHigher := 0
		for _, pos := range plots {
			size += 1
			if pos.x < xLower {
				xLower = pos.x
			}
			if pos.x > xHigher {
				xHigher = pos.x
			}
			if pos.y < yLower {
				yLower = pos.y
			}
			if pos.y > yHigher {
				yHigher = pos.y
			}
            regionMap[pos] = region
		}
		fmt.Printf("Region %d (%s) - x: %d %d; y: %d %d\n", region, string(plotMap[plots[0]]), xLower, xHigher, yLower, yHigher)

		// Top down i.e. check Upper borders
		fmt.Println("Checking upper borders")
		upperBorders := []point{}
		for _, p := range plots {
			pUpper := point{p.x, p.y - 1}
			if pUpper.y < 0 || regionMap[pUpper] != regionMap[p] {
				upperBorders = append(upperBorders, p)
                fmt.Printf("%s %s \n", pUpper, regionMap[pUpper])
			}
		}
        fmt.Printf("Starting borders: %s\n", upperBorders)
		for y := yLower; y <= yHigher; y++ {
			for x := xLower; x <= xHigher; x++ {
				p := point{x, y}
				found := contains(upperBorders, p)

				if found {
					pUpper := point{p.x, p.y - 1}
					if pUpper.y < 0 || regionMap[pUpper] != regionMap[p] {
						// We have an upper border
						// We keep this one but delete every other
						// to the right with the same open border
						fmt.Printf("Found border: %v\n", p)
						for x2 := p.x + 1; x2 <= xHigher; x2++ {
							pRight := point{x2, y}
							if contains(upperBorders, pRight) {
								pUpperRight := point{x2, y - 1}
								if pUpperRight.y < 0 || regionMap[pUpperRight] != regionMap[p] {
									upperBorders, _ = remove(upperBorders, pRight)
									fmt.Printf("  removing: %v\n", pRight)
								} else {
									break
								}
							} else {
								break
							}

						}
					} else {
						upperBorders, _ = remove(upperBorders, p)
					}
				}

			}
		}
		fmt.Printf("Upper borders: %d\n", len(upperBorders))

		// Bottom up i.e. check lower borders
		fmt.Println("Checking lower borders")
		lowerBorders := []point{}
		for _, p := range plots {
			other := point{p.x, p.y + 1}
			if other.y >= maxHeight || regionMap[other] != regionMap[p] {
				lowerBorders = append(lowerBorders, p)
			}
		}
		for y := yHigher; y >= yLower; y-- {
			for x := xLower; x <= xHigher; x++ {
				p := point{x, y}
				found := contains(lowerBorders, p)

				if found {
					pLower := point{p.x, p.y + 1}
					if pLower.y >= maxHeight || regionMap[pLower] != regionMap[p] {
						// We have an lower border
						// We keep this one but delete every other
						// to the right with the same open border
						fmt.Printf("Found border: %v\n", p)
						for x2 := p.x + 1; x2 <= xHigher; x2++ {
							pRight := point{x2, y}
							if contains(lowerBorders, pRight) {
								pLowerRight := point{x2, y + 1}
								if pLowerRight.y >= maxHeight || regionMap[pLowerRight] != regionMap[p] {
									lowerBorders, _ = remove(lowerBorders, pRight)
									fmt.Printf("  removing: %v\n", pRight)
								} else {
									break
								}
							} else {
								break
							}

						}
					} else {
						lowerBorders, _ = remove(lowerBorders, p)
					}
				}

			}
		}
		fmt.Printf("Upper borders: %d\n", len(lowerBorders))

		// Left to Right i.e. check Upper borders
		fmt.Println("Checking left borders")
		leftBorders := []point{}
		for _, p := range plots {
			other := point{p.x - 1, p.y}
			if other.x < 0 || regionMap[other] != regionMap[p] {
				leftBorders = append(leftBorders, p)
			}
		}
		for x := xLower; x <= xHigher; x++ {
			for y := yLower; y <= yHigher; y++ {
				p := point{x, y}
				found := contains(leftBorders, p)

				if found {
					pLeft := point{p.x - 1, p.y}
					if pLeft.x < 0 || regionMap[pLeft] != regionMap[p] {
						// We have an left side border
						// We keep this one but delete every other
						// to the down side with the same open border
						fmt.Printf("Found border: %v\n", p)
						for y2 := p.y + 1; y2 <= yHigher; y2++ {
							pDown := point{x, y2}
							if contains(leftBorders, pDown) {
								pLeftDown := point{x - 1, y2}
								if pLeftDown.x < 0 || regionMap[pLeftDown] != regionMap[p] {
									leftBorders, _ = remove(leftBorders, pDown)
									fmt.Printf("  removing: %v\n", pDown)
								} else {
									break
								}
							} else {
								break
							}

						}
					} else {
						leftBorders, _ = remove(leftBorders, p)
					}
				}

			}
		}
		fmt.Printf("Upper borders: %d\n", len(leftBorders))

		// Right to Left i.e. check Upper borders
		fmt.Println("Checking right borders")
		rightBorders := []point{}
		for _, p := range plots {
			other := point{p.x + 1, p.y}
			if other.x >= maxWidth || regionMap[other] != regionMap[p] {
				rightBorders = append(rightBorders, p)
			}
		}
		for x := xHigher; x >= xLower; x-- {
			for y := yLower; y <= yHigher; y++ {
				p := point{x, y}
				found := contains(rightBorders, p)

				if found {
					pRight := point{p.x + 1, p.y}
					if pRight.x >= maxWidth || regionMap[pRight] != regionMap[p] {
						// We have an left side border
						// We keep this one but delete every other
						// to the down side with the same open border
						fmt.Printf("Found border: %v\n", p)
                        for y2 := p.y + 1; y2 <= yHigher; y2++ {
                            pDown := point{x, y2}
                            if contains(rightBorders, pDown) {
                                pRightDown := point{x + 1, y2}
                                if pRightDown.x < 0 || regionMap[pRightDown] != regionMap[p] {
                                    rightBorders, _ = remove(rightBorders, pDown)
                                    fmt.Printf("  removing: %v\n", pDown)
                                } else {
                                    break
                                }

                            } else {
                                break
                            }
                        }
                    } else {
                        rightBorders, _ = remove(rightBorders, p)
                    }
				}
			}
		}
		fmt.Printf("Upper borders: %d\n", len(rightBorders))

		perimeter := len(rightBorders) + len(leftBorders) + len(upperBorders) + len(lowerBorders)

		fmt.Printf("Region %v (%s) size=%d, perimeter=%d\n", region, string(plotMap[plots[0]]), size, perimeter)
		sum += size * perimeter
	}

	fmt.Printf(" -> Sum: %d\n", sum)

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
