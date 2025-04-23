package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

type point struct {
	x, y int
}

// Define the custom type
type Direction int

// Define enum values using iota
const (
	North Direction = iota // Starts at 0
	East                   // 1
	South                  // 2
	West                   // 3
)

type path struct {
	p point
	d Direction
}

// PathSet type using map[path]struct{}
type PathSet struct {
	m map[path]struct{}
}

// NewPathSet creates a new set
func NewPathSet() *PathSet {
	return &PathSet{m: make(map[path]struct{})}
}

// Add adds an element to the set
func (s *PathSet) Add(value path) {
	s.m[value] = struct{}{}
}

// Contains checks if an element is in the set
func (s *PathSet) Contains(value path) bool {
	_, exists := s.m[value]
	return exists
}

// Remove removes an element from the set
func (s *PathSet) Remove(value path) {
	delete(s.m, value)
}

// Size returns the number of elements in the set
func (s *PathSet) Size() int {
	return len(s.m)
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

func sortSlow(slice []path, cost map[path]int) []path {
	newSlice := append([]path(nil), slice...)

	for i := 0; i < len(newSlice); i++ {
		for j := i + 1; j < len(newSlice); j++ {
			if cost[newSlice[j]] < cost[newSlice[i]] {
				tmp := newSlice[i]
				newSlice[i] = newSlice[j]
				newSlice[j] = tmp
			}
		}
	}
	return newSlice

}

func sortCopy(slice []path, cost map[path]int) []path {
	newSlice := append([]path(nil), slice...) // Copy the slice

	sort.Slice(newSlice, func(i, j int) bool {
		return cost[newSlice[i]] < cost[newSlice[j]]
	})

	return newSlice
}

func sortInPlace(slice []path, cost map[path]int) {
	sort.Slice(slice, func(i, j int) bool {
		return cost[slice[i]] < cost[slice[j]] // Sort based on cost map
	})
}

func MinSlice(slice []int) int {
	if len(slice) == 0 {
		fmt.Println("ERROR: Cant search for min on a zero length slice")
		os.Exit(1)
	}
	m := slice[0]
	for i := 1; i < len(slice); i++ {
		if slice[i] < m {
			m = slice[i]
		}
	}
	return m
}

func search(adjMap map[path]map[path]int, start path, end point, startVal int) (map[path]int, int) {
	openSet := []path{start}
	gScore := make(map[path]int)
	gScore[start] = startVal

    visited := NewPathSet()

	for len(openSet) > 0 {
		current := openSet[0]
		openSet = openSet[1:]

		visited.Add(current)

		for other, cost := range adjMap[current] {
			if !visited.Contains(other) {
				newDistance := gScore[current] + cost

				if cost, exists := gScore[other]; exists {
					if newDistance < cost {
						gScore[other] = newDistance
						openSet = append(openSet, other)
					}
				} else {
					gScore[other] = newDistance
					openSet = append(openSet, other)
				}
			}
		}
		sortInPlace(openSet, gScore)
	}
    
	t := []path{path{end, East}, path{end, North}, path{end, West}, path{end, South}}
	endVals := []int{}
	for _, endPath := range t {
		if val, exists := gScore[endPath]; exists {
			endVals = append(endVals, val)
		}
	}

	return gScore, MinSlice([]int{gScore[path{end, East}], gScore[path{end, North}], gScore[path{end, West}], gScore[path{end, South}]})
}

func printMap(tilemap map[point]bool, maxWidth, maxHeight int, visited *PointSet, start, end point) {
    for y:=0;y<=maxHeight;y++ {
        line := ""
        for x:=0;x<=maxWidth;x++ {
            p := point{x, y}
            if p == start {
                line = line + "S"
            } else if p == end {
                line = line + "E"
            } else if visited.Contains(p) {
                line = line + "O"
            } else if tilemap[p] {
                line = line + "."
            } else {
                line = line + "#"
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

	moveCost := 1
	turnCost := 1000

	var start path
	var end point

	maxWidth := 0
	maxHeight := 0

	tilemap := make(map[point]bool)

	{
		y := 0
		for scanner.Scan() {
			line := scanner.Text()
			//fmt.Println(line)

			for x, char := range line {
				p := point{x, y}
				tile := false
				switch char {
				case '#':
					tile = false
				case '.':
					tile = true
				case 'S':
					tile = true
					start = path{p, East}
				case 'E':
					tile = true
					end = p
				}
                tilemap[p] = tile
				if x > maxWidth {
					maxWidth = x + 1
				}
			}

			if y > maxHeight {
				maxHeight = y + 1
			}
			y += 1
		}
	}

	simpleAdjMap := make(map[point][]point)
	adjMap := make(map[path]map[path]int)
	// Instantiate the slices and maps.
	for x := 0; x < maxWidth; x++ {
		for y := 0; y < maxHeight; y++ {
			p := point{x, y}
			simpleAdjMap[p] = []point{}

			for d := 0; d < 4; d++ {
				adjMap[path{p, Direction(d)}] = make(map[path]int)
			}
		}
	}

	for x := 0; x < maxWidth; x++ {
		for y := 0; y < maxHeight; y++ {
			p := point{x, y}
			if tilemap[p] == false {
				continue
			}

			// Inplace rotations
			for d := 0; d < 4; d++ {
				pa := path{p, Direction(d)}
				left := (4 + d - 1) % 4
				right := (d + 1) % 4
				adjMap[pa][path{p, Direction(left)}] = turnCost
				adjMap[pa][path{p, Direction(right)}] = turnCost
			}

			// Movements
			pU := path{point{p.x, p.y - 1}, North}
			pD := path{point{p.x, p.y + 1}, South}
			pL := path{point{p.x - 1, p.y}, West}
			pR := path{point{p.x + 1, p.y}, East}

			adj := []path{pU, pD, pL, pR}
			for _, pa := range adj {
				if tilemap[pa.p] {
					simpleAdjMap[p] = append(simpleAdjMap[p], pa.p)
					adjMap[path{p, pa.d}][pa] = moveCost
				}
			}
		}
	}

	// Search the path
    gScore, finalScore := search(adjMap, start, end, 0)
    gScoreInverse, finalScoreInverse1 := search(adjMap, path{end,South}, start.p, 0)

    visited := NewPointSet()

    fmt.Printf("Score: %v\n", finalScore)
    fmt.Printf("Score: %v\n", finalScoreInverse1)

    for x:=0;x<maxWidth;x++ {
        for y:=0;y<maxHeight;y++{
            p := point{x, y}
            if !tilemap[p] {
                continue
            }
            found := false
            outer:
            for d:=0;d<4;d++{
                dir := Direction(d)

                for d2:=0;d2<4;d2++{
                    dir2 := Direction(d2)
                    if gScore[path{p, dir}] + gScoreInverse[path{p, dir2}] == finalScore{
                        visited.Add(p)
                        found = true
                    } else if gScore[path{p, dir2}] + gScoreInverse[path{p, dir}] == finalScore {
                        visited.Add(p)
                        found = true
                    }
                    if found {
                        break outer
                    }
                }
                if found {
                    break outer
                }

            }
        }
    }

    sum += visited.Size()
    printMap(tilemap, maxWidth, maxHeight, visited, start.p, end)

	fmt.Printf(" -> Sum: %d\n", sum)

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
