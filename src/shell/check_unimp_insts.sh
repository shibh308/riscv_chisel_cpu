echo "DO NOT USE THIS SCRIPT DIRECTLY"
exit

cat src/c/ctest.dump | grep $'\t' | awk '{print $3}' | sort | uniq | sed '1d' > results/ctest_insts.txt
cat src/main/scala/common/Instructions.scala | grep $'val' | awk '{print $2}' | tr '[A-Z]' '[a-z]' | sort > results/cpu_insts.txt
sort results/cpu_insts.txt results/cpu_insts.txt results/ctest_insts.txt | uniq -u > results/unimpl_insts.txt