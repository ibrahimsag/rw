set -x
echo $1
clang-8 -O1 -fno-discard-value-names -S -emit-llvm $1
