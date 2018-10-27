#!/bin/sh

# Redirect output to stderr.
exec 1>&2

set -o posix
set -o nounset
set -o pipefail
# set -o errexit

die() {
    echo "ERROR: $1, see $2: Will not accept commit"
    exit 1
}

make_clean() {
  make clean >/dev/null 2>&1
}

MAKECMD="make -j 3"

mkdir -p ../logs

CPPCHECK_LOG="../logs/cppcheck.log"
GCC_DEBUG_LOG="../logs/gcc-debug.log"
GCC_DEBUG_TEST_LOG="../logs/gcc-debug-test.log"
GCC_REL_LOG="../logs/gcc-release.log"
GCC_REL_TEST_LOG="../logs/gcc-release-test.log"
CLANG_DEBUG_LOG="../logs/clang-debug.log"
CLANG_DEBUG_TEST_LOG="../logs/clang-debug-test.log"
CLANG_REL_LOG="../logs/clang-release.log"
CLANG_REL_TEST_LOG="../logs/clang-release-test.log"

make_clean

echo "Running Cppcheck"
make cppcheck >$CPPCHECK_LOG 2>&1
if [ $? -ne 0 ]; then
    die "Cppcheck returned != 0" $CPPCHECK_LOG
fi

echo "Checking cppcheck output for warnings"
grep -v "^\[" $CPPCHECK_LOG >/dev/null 2>&1
if [ $? -ne 0 ]; then
    die "Cppcheck has warnings" $CPPCHECK_LOG
fi

make_clean

echo "Running GCC:debug build"
CXX=g++ rel=no $MAKECMD >$GCC_DEBUG_LOG 2>&1
if [ $? -ne 0 ]; then
    die "GCC:debug build returned != 0" $GCC_DEBUG_LOG
fi

echo "Checking GCC:debug output for errors"
grep -v "error" $GCC_DEBUG_LOG >/dev/null 2>&1
if [ $? -ne 0 ]; then
    die "GCC:debug build has errors" $GCC_DEBUG_LOG
fi

echo "Checking GCC:debug output for warnings"
grep -v "warning" $GCC_DEBUG_LOG >/dev/null 2>&1
if [ $? -ne 0 ]; then
    die "GCC:debug build has warnings" $GCC_DEBUG_LOG
fi

echo "Testing GCC:debug build"
make test >$GCC_DEBUG_TEST_LOG 2>&1
if [ $? -ne 0 ]; then
    die "Testing GCC:debug build returned != 0" $GCC_DEBUG_TEST_LOG
fi

make_clean

echo "Running GCC:release build"
CXX=g++ rel=yes $MAKECMD >$GCC_REL_LOG 2>&1
if [ $? -ne 0 ]; then
    die "GCC:release build returned != 0" $GCC_REL_LOG
fi

echo "Checking GCC:release output for errors"
grep -v "error" $GCC_REL_LOG >/dev/null 2>&1
if [ $? -ne 0 ]; then
    die "GCC:release build has errors" $GCC_REL_LOG
fi

echo "Checking GCC:release output for warnings"
grep -v "warning" $GCC_REL_LOG >/dev/null 2>&1
if [ $? -ne 0 ]; then
    die "GCC:release build has warnings" $GCC_REL_LOG
fi

echo "Testing GCC:release build"
make test >$GCC_REL_TEST_LOG 2>&1
if [ $? -ne 0 ]; then
    die "Testing GCC:release build returned != 0" $GCC_REL_TEST_LOG
fi

make_clean

echo "Running Clang:debug build"
CXX=clang++ rel=no $MAKECMD >$CLANG_DEBUG_LOG 2>&1
if [ $? -ne 0 ]; then
    die "Clang:debug build returned != 0" $CLANG_DEBUG_LOG
fi

echo "Checking Clang:debug output for errors"
grep -v "error" $CLANG_DEBUG_LOG >/dev/null 2>&1
if [ $? -ne 0 ]; then
    die "Clang:debug build has errors" $CLANG_DEBUG_LOG
fi

echo "Checking Clang:debug output for warnings"
grep -v "warning" $CLANG_DEBUG_LOG >/dev/null 2>&1
if [ $? -ne 0 ]; then
    die "Clang:debug build has warnings" $CLANG_DEBUG_LOG
fi

echo "Testing Clang:debug build"
make test >$CLANG_DEBUG_TEST_LOG 2>&1
if [ $? -ne 0 ]; then
    die "Testing Clang:debug build returned != 0" $CLANG_DEBUG_TEST_LOG
fi

make_clean

echo "Running Clang:release build"
CXX=clang++ rel=yes $MAKECMD >$CLANG_REL_LOG 2>&1
if [ $? -ne 0 ]; then
    die "Clang:release build returned != 0" $CLANG_REL_LOG
fi

echo "Checking Clang:release output for errors..."
grep -v "error" $CLANG_REL_LOG >/dev/null 2>&1
if [ $? -ne 0 ]; then
    die "Clang:release build has errors" $CLANG_REL_LOG
fi

echo "Checking Clang:release output for warnings..."
grep -v "warning" $CLANG_REL_LOG >/dev/null 2>&1
if [ $? -ne 0 ]; then
    die "Clang:release build has warnings" $CLANG_REL_LOG
fi

echo "Testing Clang:release build"
make test >$CLANG_REL_TEST_LOG 2>&1
if [ $? -ne 0 ]; then
    die "Testing Clang:release build returned != 0" $CLANG_REL_TEST_LOG
fi

make_clean

echo "All good"
exit 0
