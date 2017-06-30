## Prerequisites

```
go get golang.org/x/tools/cmd/stringer  # prerequisite
stringer -type Op op.go
```

## Build

```
go build -o um *.go
```

### Run

```
./um ../sandmark.umz  # run the given program
./um -disassemble ../sandmark.umz  # look at the given program
./um -rtl ../sandmark.umz  # RTL-like semantics for the given program
```
