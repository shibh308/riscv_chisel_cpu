# RISCV-Chisel-CPU

## Usage

### riscv-tests
```
bash src/shell/riscv_tests.sh
bash src/shell/show_test_results.sh
```

### ctest
```
sh src/shell/compile_riscv.sh src/c/ctest src/c/link.ld
sbt "testOnly mycpu.HexTest"
sh src/shell/clean_compiled_file.sh src/c/ctest
```