## Draft VM in Go

### Prerequisites

```shell
go get golang.org/x/tools/cmd/stringer
stringer -type Op op.go
```

### Build

```shell
go build -o um *.go
```

### Run

```shell
./um  # usage message
./um ../hello.um
./um -disassemble ../hello.um
./um -rtl ../hello.um
```
