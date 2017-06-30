## Prerequisites

```
go get golang.org/x/tools/cmd/stringer
stringer -type Op op.go
```

## Build

```
go build -o um *.go
```

### Run

```
./um  # usage message
./um ../sandmark.umz
./um -disassemble ../sandmark.umz
./um -rtl ../sandmark.umz
```
