package main

import (
	"bufio"
	"fmt"
	"os"
	//"sync"
	"strconv"
	"strings"
    "sort"
)

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

// PointSet type using map[point]struct{}
type PointSet struct {
	m map[point]struct{}
}

// NewPointSet creates a new set
func NewPointSet() *PointSet {
	return &PointSet{m: make(map[point]struct{})}
}

// Add adds an element to the set
func (s *PointSet) Add(value point) {
	s.m[value] = struct{}{}
}

// Contains checks if an element is in the set
func (s *PointSet) Contains(value point) bool {
	_, exists := s.m[value]
	return exists
}

// Remove removes an element from the set
func (s *PointSet) Remove(value point) {
	delete(s.m, value)
}

// Size returns the number of elements in the set
func (s *PointSet) Size() int {
	return len(s.m)
}

func appendIfNew[T comparable](slice []T, element T) ([]T, bool) {
	for _, other := range slice {
		if other == element {
			return slice, false
		}
	}
	return append(slice, element), true
}

type point struct {
	x, y int
}

func readInt(s string) int {
	val, err := strconv.Atoi(s)
	if err != nil {
		//fmt.Printf("Error converting str %s to int\n", s)
		os.Exit(1)
	}
	return val
}

func sortInPlace(slice []point) {
	sort.Slice(slice, func(i, j int) bool {
		return slice[i].x+slice[i].y < slice[j].x+slice[j].y
	})
}

func findShortestPath(maxWidth, maxHeight int, corruption []point, start, end point, fallenBytes int) int {
	tilemap := make(map[point]bool)
	adjMap := make(map[point]map[point]bool)
	gScore := make(map[point]int)
    cameFrom := make(map[point]point)

	for x := 0; x <= maxWidth; x++ {
		for y := 0; y <= maxHeight; y++ {
			p := point{x, y}
			gScore[p] = int(^uint(0) >> 1)
			tilemap[p] = false
			adjMap[p] = make(map[point]bool)
			for x2 := 0; x2 < maxWidth; x2++ {
				for y2 := 0; y2 < maxHeight; y2++ {
					p2 := point{x2, y2}
					adjMap[p][p2] = false
				}
			}
		}
	}
    fmt.Printf("len corruption %v\n", len(corruption[0:fallenBytes]))
	for i := 0; i < Min(fallenBytes, len(corruption)); i++ {
		p := corruption[i]
		tilemap[p] = true
	}

	for x := 0; x <= maxWidth; x++ {
		for y := 0; y <= maxHeight; y++ {
			p := point{x, y}
			if !tilemap[p] {
				adjacants := []point{}
                adjacants = append(adjacants, point{x-1, y})
                adjacants = append(adjacants, point{x+1, y})
                adjacants = append(adjacants, point{x, y-1})
                adjacants = append(adjacants, point{x, y+1})
				for _, other := range adjacants {
                    if other.x >= 0 && other.x <= maxWidth && other.y >= 0 && other.y <= maxHeight {
                        if !tilemap[other] {
                            adjMap[p][other] = true
                            adjMap[other][p] = true
                        }
                    }
				}
			}
		}
	}

	visited := NewPointSet()
	// Adjacancy map is done
	gScore[start] = 0
	toWork := []point{start}

    //fmt.Printf("AdjMap %v\n", adjMap)

	for len(toWork) > 0 {
		current := toWork[0]
		toWork = toWork[1:]

		visited.Add(current)

        //fmt.Printf("Current %v\n", current)

		if current == end {
			break
		}

		for other, connected := range adjMap[current] {
			if connected {
				distance := gScore[current] + 1
				if distance < gScore[other] {
					gScore[other] = distance
                    cameFrom[other] = current
					if !visited.Contains(other) {
						toWork = append(toWork, other)
					}
				}

			}
		}
		sortInPlace(toWork)
	}
    //for key, val := range gScore {
    //    fmt.Printf("gScore %v %v\n", key, val)
    //}

    current := end
    walked := []point{end}
    for true {
        //fmt.Printf(" - %v\n", current)
        next := cameFrom[current]
        walked = append(walked, next)
        if next == start {
            break

        }
        current = next
    }

	return gScore[end]
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

	maxWidth := 6 //70
	maxHeight := 6 // 70

	corruption := []point{}
	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)
		splits := strings.Split(line, ",")
		p := point{readInt(splits[0]), readInt(splits[1])}
		//tilemap[p] = true
		corruption = append(corruption, p)
	}

	start := point{0, 0}
	end := point{maxWidth, maxHeight}

	// findShortestPath(maxWidth, maxHeight int, corruption []point, start, end point, fallenBytes int) int {

    maxInt := int(^uint(0) >> 1)
    count := 12
    sum = findShortestPath(maxWidth, maxHeight, corruption, start, end, count)
    fmt.Printf("Count %v, sum %v\n", count, sum)
    countUp := 10
    for sum < maxInt {
        count += countUp
        sum = findShortestPath(maxWidth, maxHeight, corruption, start, end, count)
        fmt.Printf("Count %v, sum %v\n", count, sum)
    }

    for sum == maxInt {
        count -= 2
        sum = findShortestPath(maxWidth, maxHeight, corruption, start, end, count)
        fmt.Printf("Count %v, sum %v\n", count, sum)
    }


    for sum < maxInt {
        count += 1
        sum = findShortestPath(maxWidth, maxHeight, corruption, start, end, count)
        fmt.Printf("Count %v, sum %v\n", count, sum)
    }


	fmt.Printf(" -> Sum: %d\n", corruption[count])

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
