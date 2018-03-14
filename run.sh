#!/bin/bash

cat "${1}" | erl -noshell -run tarry main -run init stop
