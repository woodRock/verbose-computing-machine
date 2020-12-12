#! /bin/bash

echo -e "---\nExecuting Tests\n---"

test () {
    expected=$2
    actual=`echo "$1" | runhaskell part-1.hs`
    if [[ $expected == $actual ]]; then
        echo "Pass!"
    else
        echo -e "Failed\nActual: $actual, Expected: $expected"
    fi
}

test_II () {
    expected=$2
    actual=`echo "$1" | runhaskell part-2.hs`
    if [[ $expected == $actual ]]; then
        echo "Pass!"
    else
        echo -e "Failed\nActual: $actual, Expected: $expected"
    fi
}

file=`cat test-1`
test "$file" "11"

file=`cat test-2`
test_II "$file" "6"

