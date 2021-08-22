# dsl_map

**dsl_map** is a stl-compatible implementation of a separate-chaining hashmap. The hashmap relies on the default hash function `std::hash` for a given class `Key`, makes `Key` comparisons via `std::equal_to`, and obeys a power of two growth policy.

Being stl-compatible, the implementation makes use of `pmr::polymorphic_allocator` to `std::byte` introduced in **C++17**, and a templated iterator interface for bucket-to-bucket and node-to-node traversal. 

## Toolchain
* Environment: `cygwin64 v3.2.0`
* CMake: `v3.19.2`
* Compiler: `gcc 10`
* Debugger: `gdb v9.2`
* CXX_Standard: `C++20`

## TODO
* Unit Testing with `googletest`
* Lightweight benchmarking with google benchmark
* Greater adherence to the container specifications and named requirements for `std::unordered_map`
