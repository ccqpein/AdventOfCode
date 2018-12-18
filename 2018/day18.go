package main

import (
	"bufio"
	"fmt"
	"os"
	"sync"
)

func readFile(path string) [][]byte {
	result := [][]byte{}
	file, _ := os.Open(path)
	fscanner := bufio.NewScanner(file)
	for fscanner.Scan() {
		//result = append(result, fscanner.Bytes())
		result = append(result, []byte(fscanner.Text()))
	}
	return result
}

func eachLine(thisL, prevL, nextL []byte) []byte {
	others := []byte{}
	newL := []byte{}

	for i := 0; i < len(thisL); i++ {
		if i-1 >= 0 {
			others = append(others, thisL[i-1])
			if prevL != nil {
				others = append(others, prevL[i-1])
			}
			if nextL != nil {
				others = append(others, nextL[i-1])
			}
		}
		if i+1 < len(thisL) {
			others = append(others, thisL[i+1])
			if prevL != nil {
				others = append(others, prevL[i+1])
			}
			if nextL != nil {
				others = append(others, nextL[i+1])
			}
		}

		if prevL != nil {
			others = append(others, prevL[i])
		}
		if nextL != nil {
			others = append(others, nextL[i])
		}
		//fmt.Printf("%s\n", others)

		newL = append(newL, eachPoint(thisL[i], others))
		others = []byte{}

	}
	return newL
}

func eachPoint(this byte, others []byte) byte {

	treeNum := howMany(others, []byte("|")[0])
	lumberNum := howMany(others, []byte("#")[0])

	if this == []byte(".")[0] {
		if treeNum >= 3 {
			return []byte("|")[0]
		} else {
			return this
		}
	}

	if this == []byte("|")[0] {
		if lumberNum >= 3 {
			return []byte("#")[0]
		} else {
			return this
		}
	}

	if this == []byte("#")[0] {
		if treeNum >= 1 && lumberNum >= 1 {
			return this
		} else {
			return []byte(".")[0]
		}
	}
	return 100
}

func howMany(points []byte, which byte) int {
	re := 0
	for _, p := range points {
		if p == which {
			re++
		}
	}
	return re
}

//50 lines, wholeNum = 49
func singleStep(re map[int][]byte, wholeNum int) map[int][]byte {
	result := map[int][]byte{}
	tempresult := sync.Map{}

	var finish = make(chan int)
	for i := 0; i <= wholeNum; i++ {
		ind := i
		go func(finish chan int, ind int) {
			if ind == 0 {
				tempresult.Store(ind, eachLine(re[ind], nil, re[ind+1]))
			} else if ind == wholeNum {
				tempresult.Store(ind, eachLine(re[ind], re[ind-1], nil))
			} else {
				tempresult.Store(ind, eachLine(re[ind], re[ind-1], re[ind+1]))
			}
			finish <- 1
		}(finish, ind)
	}

	for i := 0; i <= wholeNum; i++ {
		<-finish
	}

	for i := 0; i <= wholeNum; i++ {
		if v, ok := tempresult.Load(i); ok {
			result[i] = v.([]byte)
		} else {
			panic("aa")
		}
	}

	return result
}

func test() {
	//test
	filecache := readFile("./day18.input.demo")
	resultMap := map[int][]byte{}
	for i := 0; i < 10; i++ {
		resultMap[i] = filecache[i]
	}

	//fmt.Printf("%s\n", eachLine(filecache[0], nil, filecache[1]))

	for i := 0; i < 10; i++ {
		resultMap = singleStep(resultMap, 9)
		fmt.Printf("%d:\n", i+1)
		for ii := 0; ii < 10; ii++ {
			fmt.Printf("%s\n", resultMap[ii])

		}
	}

	treeNum := 0
	lumberNum := 0
	for i := 0; i < 10; i++ {
		for _, v := range resultMap[i] {
			if v == []byte("|")[0] {
				treeNum++
			}
			if v == []byte("#")[0] {
				lumberNum++
			}
		}
		fmt.Printf("%s\n", resultMap[i])
	}

	println(treeNum)
	println(lumberNum)
	println(treeNum * lumberNum)
}

func part1() {
	///part1
	filecache := readFile("./day18.input")
	resultMap := map[int][]byte{}
	for i := 0; i < 50; i++ {
		resultMap[i] = filecache[i]
	}

	for i := 0; i < 10; i++ {
		resultMap = singleStep(resultMap, 49)
	}

	treeNum := 0
	lumberNum := 0
	for i := 0; i < 50; i++ {
		for _, v := range resultMap[i] {
			if v == []byte("|")[0] {
				treeNum++
			}
			if v == []byte("#")[0] {
				lumberNum++
			}
		}
		fmt.Printf("%s\n", resultMap[i])
	}

	println(treeNum * lumberNum)
}

func part2() {
	///part1
	filecache := readFile("./day18.input")
	resultMap := map[int][]byte{}
	for i := 0; i < 50; i++ {
		resultMap[i] = filecache[i]
	}

	for i := 0; i < 1000000000; i++ {
		resultMap = singleStep(resultMap, 49)
	}

	treeNum := 0
	lumberNum := 0
	for i := 0; i < 50; i++ {
		for _, v := range resultMap[i] {
			if v == []byte("|")[0] {
				treeNum++
			}
			if v == []byte("#")[0] {
				lumberNum++
			}
		}
		fmt.Printf("%s\n", resultMap[i])
	}

	println(treeNum * lumberNum)
}

func main() {
	//test()
	//part1()
	part2()
	//filecache := readFile("./day18.input")
	//fmt.Printf("%s\n", filecache[0])
	//fmt.Printf("%s\n", filecache[1])
	//fmt.Printf("%s\n", filecache[49])
	//println(eachLine(filecache[0], nil, filecache[1]))

}
