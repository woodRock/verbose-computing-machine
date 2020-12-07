#! /bin/bash

echo -e "---\nExecuting Tests\n---"

echo "BFFFBBFRRR" | runhaskell part-1.hs
echo -e "\nExpected: 567\n"

echo "FFFBBBFRRR" | runhaskell part-1.hs
echo -e "\nExpected: 119\n"

echo "BBFFBBFRLL" | runhaskell part-1.hs
echo -e "\nExpected: 820\n"
