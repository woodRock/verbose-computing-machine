#! /bin/bash

echo -e "---\nExecuting Tests\n---"

test () {
    actual=`echo $1 | runhaskell part-1.hs`
    expected=$2
    if [[ $actual == $expected ]]; then
        echo "Pass!"
    else
        echo "Actual: $actual, Expected: $expected"
    fi
}

test "BFFFBBFRRR" "567"
test "FFFBBBFRRR" "119"
test "BBFFBBFRLL" "820"
