package main

import (
	"bufio"
	"fmt"
	"os"
	//"sync"
	"sort"
	"strconv"
	"strings"
)

// StringSet type using map[string]struct{}
type StringSet struct {
	m map[string]struct{}
}

// NewStringSet creates a new set
func NewStringSet() *StringSet {
	return &StringSet{m: make(map[string]struct{})}
}

// Add adds an element to the set
func (s *StringSet) Add(value string) {
	s.m[value] = struct{}{}
}

// Contains checks if an element is in the set
func (s *StringSet) Contains(value string) bool {
	_, exists := s.m[value]
	return exists
}

// Remove removes an element from the set
func (s *StringSet) Remove(value string) {
	delete(s.m, value)
}

// Size returns the number of elements in the set
func (s *StringSet) Size() int {
	return len(s.m)
}

// GetElements retrieves all elements in the set
func (s *StringSet) GetElements() []string {
	elements := []string{}
	for p := range s.m {
		elements = append(elements, p)
	}
	return elements
}

func readInt(s string) int {
	val, err := strconv.Atoi(s)
	if err != nil {
		//fmt.Printf("Error converting str %s to int\n", s)
		os.Exit(1)
	}
	return val
}

func getAllTriplets(adj map[string][]string, connected map[string]map[string]bool) [][]string {
	triplets := NewStringSet()

	for p1, others := range adj {
		for _, p2 := range others {
			for _, p3 := range adj[p2] {
				if connected[p1][p3] {
					triplet := []string{p1, p2, p3}
					sort.Strings(triplet)
					triplets.Add(strings.Join(triplet, ","))
				}
			}
		}
	}
	result := triplets.GetElements()
	sort.Strings(result)
	finalResult := [][]string{}
	for _, res := range result {
		triplet := strings.Split(res, ",")
		sort.Strings(triplet)
		finalResult = append(finalResult, triplet)
	}
	return finalResult
}

// Generic contains function
func contains[T comparable](slice []T, element T) bool {
	for _, item := range slice {
		if item == element {
			return true
		}
	}
	return false
}

func getMaxNetwork(adj map[string][]string, connected map[string]map[string]bool) string {
    masterlist := [][]string{}

    for node, others := range adj {
        list := append([]string{node}, others...)
        masterlist = append(masterlist, list)
    }
	// Sort by the length of the inner slices
	sort.Slice(masterlist, func(i, j int) bool {
		return len(masterlist[i]) > len(masterlist[j])
	})
    candidates := NewStringSet()
    for _, neighbours := range masterlist {
        counter := make(map[string]int)
        for i, node := range neighbours {
            counter[node] = 0
            for j, node2 := range neighbours {
                if i == j {
                    continue
                }
                if connected[node2][node] {
                    counter[node] += 1
                }
            }
        }
        counterOverview := make(map[int]int)
        for _, val := range counter {
            counterOverview[val] = 0
        }
        for _, val := range counter {
            counterOverview[val] += 1
        }
        fmt.Printf("List %v with counters %v (%v)\n", neighbours, counter, counterOverview)
        if counterOverview[12] == 12 {
            for _, n := range neighbours {
                if counter[n] == 12 {
                    candidates.Add(n)
                }
            }
        }
    }
    fmt.Printf("Candidates: %v\n", candidates.GetElements())
    target := candidates.GetElements()
    sort.Strings(target)
    return strings.Join(target, ",")


    
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

	adj := make(map[string][]string)
	connected := make(map[string]map[string]bool)

	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)

		splits := strings.Split(line, "-")
		p1 := splits[0]
		p2 := splits[1]

		if _, exists := adj[p1]; exists {
			adj[p1] = append(adj[p1], p2)
		} else {
			adj[p1] = []string{p2}
		}

		if _, exists := adj[p2]; exists {
			adj[p2] = append(adj[p2], p1)
		} else {
			adj[p2] = []string{p1}
		}

	}
	for p1, others := range adj {
		connected[p1] = make(map[string]bool)
		for p2, _ := range adj {
			connected[p1][p2] = false
		}
		for _, p2 := range others {
			connected[p1][p2] = true
		}
	}

	//func getMaxNetwork(adj map[string][]string, connected map[string]map[string]bool) string {
	result := getMaxNetwork(adj, connected)

	fmt.Println(result)

	if err4 := scanner.Err(); err4 != nil {
		fmt.Printf("Error reading input file: %v\n", err4)
		os.Exit(1)
	}

	fmt.Println("Processing complete!")
}
