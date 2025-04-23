package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
    "sync"
)

type point struct {
	x, y int
}

type skip struct {
    p1, p2 point
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

// GetElements retrieves all elements in the set
func (s *PointSet) GetElements() []point {
	elements := []point{}
	for p := range s.m {
		elements = append(elements, p)
	}
	return elements
}

// SkipSet type using map[skip]struct{}
type SkipSet struct {
	m map[skip]struct{}
}

// NewSkipSet creates a new set
func NewSkipSet() *SkipSet {
	return &SkipSet{m: make(map[skip]struct{})}
}

// Add adds an element to the set
func (s *SkipSet) Add(value skip) {
	s.m[value] = struct{}{}
}

// Contains checks if an element is in the set
func (s *SkipSet) Contains(value skip) bool {
	_, exists := s.m[value]
	return exists
}

// Remove removes an element from the set
func (s *SkipSet) Remove(value skip) {
	delete(s.m, value)
}

// Size returns the number of elements in the set
func (s *SkipSet) Size() int {
	return len(s.m)
}

// GetElements retrieves all elements in the set
func (s *SkipSet) GetElements() []skip {
	elements := []skip{}
	for p := range s.m {
		elements = append(elements, p)
	}
	return elements
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

func sortInPlace(slice []point, cost map[point]int) {
	sort.Slice(slice, func(i, j int) bool {
		return cost[slice[i]] < cost[slice[j]] // Sort based on cost map
	})
}

// Min returns the smaller of two values
func Min[T ~int | ~float64 | ~string](a, b T) T {
    if a < b {
        return a
    }
    return b
}

// Max returns the larger of two values
func Max[T ~int | ~float64 | ~string](a, b T) T {
    if a > b {
        return a
    }
    return b
}

// ManhattanDistance calculates the Manhattan distance between two points
func ManhattanDistance(p1, p2 point) int {
	return abs(p1.x-p2.x) + abs(p1.y-p2.y)
}

// Helper function to calculate the absolute value
func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}


func search(tilemapOrig map[point]bool, maxWidth, maxHeight int, start, end point, skip skip) int {
    tilemap := make(map[point]bool)
    for key, val := range tilemapOrig {
        tilemap[key] = val
    }
    
    skipStart := skip.p1
    skipEnd := skip.p2

    minX := Min(skipStart.x, skipEnd.x)
    maxX := Max(skipStart.x, skipEnd.x)
    minY := Min(skipStart.y, skipEnd.y)
    maxY := Max(skipStart.y, skipEnd.y)
    
    for x:=minX;x<=maxX;x++{
        for y:=minY;y<=maxY;y++{
            skip := point{x, y}
            tilemap[skip] = true
        }
    }


    adjMap := make(map[point]map[point]int)
    directionVectors := []point{point{0, 1}, point{0, -1}, point{1, 0}, point{-1, 0}}

    // Instantiate the slices and maps.
    for x := 0; x < maxWidth; x++ {
        for y := 0; y < maxHeight; y++ {
            p := point{x, y}
            adjMap[p] = make(map[point]int)
        }
    }

    for x := 0; x < maxWidth; x++ {
        for y := 0; y < maxHeight; y++ {
            p := point{x, y}
            if tilemap[p] == false {
                continue
            }

            for _, dv := range directionVectors {
                p2 := point{p.x + dv.x, p.y + dv.y}
                if validPoint(p2, maxWidth, maxHeight) && tilemap[p2] {
                    adjMap[p][p2] = 1
                }
            }
        }
    }
	openList := []point{start}
	gScore := make(map[point]int)
	gScore[start] = 0

    visited := NewPointSet()

	for len(openList) > 0 {
		current := openList[0]
		openList = openList[1:]

		visited.Add(current)

		for other, cost := range adjMap[current] {
			if !visited.Contains(other) {
				newDistance := gScore[current] + cost

				if gCost, exists := gScore[other]; exists {
					if newDistance < gCost {
						gScore[other] = newDistance
						openList = append(openList, other)
					}
				} else {
					gScore[other] = newDistance
					openList = append(openList, other)
				}
			}
		}
		sortInPlace(openList, gScore)
	}

    return gScore[end]
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

func validPoint(p point, maxWidth, maxHeight int) bool {
    if p.x < 0 || p.x >= maxWidth {
        return false
    }
    if p.y < 0 || p.y >= maxHeight {
        return false
    }
    return true
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

	var start point
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
					start = p
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

    skips := NewSkipSet()
    skipsIgnore := NewSkipSet()
    path := NewPointSet()
    skipsPoints := make(map[skip]int)
    for p, passable := range tilemap {
        if passable {
            path.Add(p)
        }
    }
    pathSlice := path.GetElements()
    for distance:=2;distance<=70;distance++{
        for i:=0;i<len(pathSlice);i++ {
            for j:=i+1;j<len(pathSlice);j++{
                p1 := pathSlice[i]
                p2 := pathSlice[j]
                skip1 := skip{p1, p2}
                skip2 := skip{p2, p1}
                if !skipsIgnore.Contains(skip1) && !skipsIgnore.Contains(skip2){
                    if ManhattanDistance(p1, p2) <= distance {
                        skips.Add(skip1)

                        skipStart := p1
                        skipEnd := p2

                        minX := Min(skipStart.x, skipEnd.x)
                        maxX := Max(skipStart.x, skipEnd.x)
                        minY := Min(skipStart.y, skipEnd.y)
                        maxY := Max(skipStart.y, skipEnd.y)
                        count := 0
                        for x:=minX;x<=maxX;x++{
                            for y:=minY;y<=maxY;y++{
                                count += 1
                                skipBetween := skip{p1,point{x, y}}
                                skipsIgnore.Add(skipBetween)
                                skipBetween = skip{point{x, y}, p1}
                                skipsIgnore.Add(skipBetween)
                                skipBetween = skip{p2,point{x, y}}
                                skipsIgnore.Add(skipBetween)
                                skipBetween = skip{point{x, y}, p2}
                                skipsIgnore.Add(skipBetween)
                            }
                        }
                        skipsPoints[skip1] = count
                    }
                }
            }
        }
    }



	// Search the path
    finalScore := search(tilemap, maxWidth, maxHeight, start, end, skip{start, start})
    fmt.Printf("Score: %v\n", finalScore)
    fmt.Printf("Checking skips %v\n", skips.Size())

    worker := func(id int, jobs <-chan skip, results chan<- int, wg *sync.WaitGroup) {
        defer wg.Done()

        for job := range jobs {
            result := search(tilemap, maxWidth, maxHeight, start, end, job)
            scoreDelta := finalScore - result
            //fmt.Printf("Skipping at %v => %d (d=%d)\n", s, score, scoreDelta)
            count := 0
            if scoreDelta >= 100 {
                count += skipsPoints[job]
            }
            fmt.Printf("Worker %d processed job: %d -> %d (for %v points)\n", id, job, result)
            results <- count
        }
    }

	// Inputs and setup
	jobs := make(chan skip, skips.Size())
	results := make(chan int, skips.Size())
	var wg sync.WaitGroup

	// Start workers
	numWorkers := 14
	for i := 1; i <= numWorkers; i++ {
		wg.Add(1)
		go worker(i, jobs, results, &wg)
	}

	// Send jobs
	for _, input := range skips.GetElements()  {
		jobs <- input
	}
	close(jobs)

	// Wait and collect results
	wg.Wait()
	close(results)

	for score := range results {
        sum += score
	}

	fmt.Printf(" -> Sum: %d\n", sum)

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
