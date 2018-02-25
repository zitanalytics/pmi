How to run .go on macOS to generate the simplified version of Surroundings.json

1. Extract UseCase_3_Datasets.zip
2. Copy Surroundings.json file into golang folder 
3. Go to `www.golang.org` and download Go
4. Follow the instruction to install Go
5. Open `Terminal`
6. Navigate to the folder which contains all the files (golang folder)
7. Import gabs package from `github` (type `go get "gitHub.com/jeffail/gabs"`)
8. Type `go run redojson.go "path of the Surroundings.json" > "name of output file"`
9. Output file will be written to the same folder as the original file

See `go_run_json.png` to replicate the process.

The surroundings with at least of one sublist are kept, in other words, this is the amenities that are present around each store_code
