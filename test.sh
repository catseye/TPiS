#!/bin/sh

csi -q -b test.scm > test-output.txt || exit 1
#huski test.scm > test-output.txt || exit 1   # THIS SAYS IT PASSES BUT ACTUALLY FAILS
#plt-r5rs test.scm > test-output.txt || exit 1
cat test-output.txt
if grep -q FAILED test-output.txt; then
    echo "FAILED"
    rm test-output.txt
    exit 1
else
    echo "PASSED"
    rm test-output.txt
    exit 0
fi
