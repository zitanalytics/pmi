package main

import (
	"os"
	"fmt"
	"io/ioutil"

	"github.com/Jeffail/gabs"
	"encoding/json"
)


func main() {
	if len(os.Args) != 2 {
		fmt.Println("missing required parameter: json path")
		os.Exit(1)
	}
	//fmt.Println("reading file: ", os.Args[1])
	contents, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		fmt.Print(err.Error())
		os.Exit(1)
	}

	jsonParsed, err := gabs.ParseJSON(contents)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}

	children, err := jsonParsed.Children()
	if err != nil {
		fmt.Println("cannot read list of children of the root json object: " + err.Error())
		os.Exit(1)
	}

	result := map[string]map[string]int{}
	for _, child := range children {
		storeCode := child.S("store_code").Data().(float64)
		surroundings := child.S("surroundings")
		surroundingsChildren, err := surroundings.ChildrenMap()
		if err != nil {
			fmt.Printf("cannot read list of surroundings for %s: %s\n", storeCode, err)
			os.Exit(1)
		}

		scount := map[string]int{}
		for key, child := range surroundingsChildren {
			arr, ok := child.Data().([]interface{})
			if !ok {
				fmt.Printf("cannot read list of surroundings as array %s: %s:%s\n", storeCode, key, err)
				os.Exit(1)
			}
			//fmt.Printf("\t%s => %d\n", key, len(arr))
			n := len(arr)
			if n > 0 {
				scount[key] = n
			}
		}
		result[fmt.Sprintf("%d", int(storeCode))] = scount
	}

	resultJSON, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		fmt.Printf("cannot marshal result as JSON: %s", err.Error())
		os.Exit(1)
	}
	fmt.Println(string(resultJSON))
}
