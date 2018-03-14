#!/bin/bash

cat "${1}" | erl -noshell -run main main -run init stop
