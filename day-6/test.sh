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

file=`cat test`
test "$file" "11"

