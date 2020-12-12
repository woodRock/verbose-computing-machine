#! /bin/bash

test () {
    expected=$2
    actual=`echo "$1" | runhaskell part-2.hs`
    if [[ $actual == $expected ]]; then
        echo "Pass!"
    else
        echo -e "Fail: Actual: $actual, Expected: $expected"
    fi
}

echo -e "---\nExecuting Tests\n---"

file=`cat test-1`
test "$file" "32"

file=`cat test-2`
test "$file" "126"

file=`cat input.txt`
test "$file" "158493"
