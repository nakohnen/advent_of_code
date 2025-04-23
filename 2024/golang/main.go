package main

import (
	"fmt"
    "os"
)

func encodeMoves(line string) int {
    if len(line) == 0 {
        return 0
    } else if len(line) == 1 {
        switch line {
        case "<":
            return 1
        case "v":
            return 2
        case ">":
            return 3
        case "^":
            return 4
        case "A":
            return 5
        }
    } 
    return 10 * encodeMoves(string(line[:len(line)-1])) + encodeMoves(string(line[len(line)-1]))
}

func decodeMoves(move int) string {
    line := ""
    rest := move % 10
    next := move / 10
    for rest > 0 {
        l := ""
        switch rest {
            case 5:
                l = "A"
            case 4:
                l = "^"
            case 3:
                l = ">"
            case 2:
                l = "v"
            case 1:
                l = "<"
            default:
                os.Exit(1)
        }
        line = l + line
        rest = next % 10
        next = next / 10
    }
    return line
}

func main() {
	// Example input
    example := "<^>vA"
    fmt.Printf("%v %v\n", decodeMoves(encodeMoves(example)), example)
}
