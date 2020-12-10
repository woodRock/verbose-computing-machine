#! /bin/bash

echo -e "---\nExecuting tests\n---"

test () {
    expected=$2
    actual=`echo "${1}" | runhaskell part-1.hs`

    if [[ $actual == $expected ]]; then
        echo "Pass!"
    else
        echo "Fail: Actual: ${actual}, Expected: ${expected}"
    fi
}

test_2 () {
    expected=$2
    actual=`echo "${1}" | runhaskell part-2.hs`

    if [[ $actual == $expected ]]; then
        echo "Pass!"
    else
        echo "Fail: Actual: ${actual}, Expected: ${expected}"
    fi
}

file=`cat test`
test "${file}" "127"
test_2 "${file}" "62"
