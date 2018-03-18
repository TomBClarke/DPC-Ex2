# DPC-Ex2
UoB CS DPC Assessment 2 - Distribution

## Compiling

The code has been written and tested for Erlang 20.2.  
To compile the code, run from this directory:
```bash
erlc *.erl
```

## Running

To run the code, execute from this directory:
```bash
erl -noshell -run tarry main -run init stop
```
and pipe into `stdin` the input data.

The script [run.sh](run.sh) has been provided which takes an input file as an aregument and pipes it into the program for you.

## Testing

To test the code with the well-formed input data in [example_data/](example_data/) and the malformed input data in [bad_inputs/](bad_inputs/) run the testing script [test_all.sh](test_all.sh).
