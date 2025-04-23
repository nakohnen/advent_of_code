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
            regionMap[pos] = y * maxWidth + x 

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

                    if len(candidates)==0 && len(newStartingPositions) > 0 {
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
		perimeter := 0
		for _, pos := range plots {
			size += 1
			perimeter += 4 - len(neighboursPlot[pos])
		}
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
