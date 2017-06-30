```
go get golang.org/x/tools/cmd/stringer
go generate
go build -o um *.go
./um ../sandmark.umz  # run the given program
./um -disassemble ../sandmark.umz  # look at the given program
./um -rtl ../sandmark.umz  # RTL-like semantics for the given program
```
