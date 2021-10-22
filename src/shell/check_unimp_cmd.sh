echo "DO NOT USE THIS SCRIPT DIRECTLY"
exit

cat src/c/ctest.dump | grep $'\t' | awk '{print $3}' | sort | uniq | sed '1d' > test_run_dir/ctest_insts.txt
cat src/main/scala/common/Instructions.scala | grep $'val' | awk '{print $2}' | tr '[A-Z]' '[a-z]' | sort > test_run_dir/cpu_insts.txt
sort test_run_dir/cpu_insts.txt test_run_dir/cpu_insts.txt test_run_dir/ctest_insts.txt | uniq -u > test_run_dir/unimpl_insts.txt