cmake_minimum_required(VERSION 3.27)
project(rustint)

# Set C++ standard to 23
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Enable testing
enable_testing()

# Compiler-specific flags
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    add_compile_options(-std=c++23 -fconcepts)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_compile_options(-std=c++23 -stdlib=libc++)
    add_link_options(-stdlib=libc++)
endif()

add_library(rustint INTERFACE)
target_include_directories(
  rustint INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
                    $<INSTALL_INTERFACE:include>)

target_compile_features(rustint INTERFACE cxx_std_23)

# Test executable
add_executable(test_rustint tests/test_rustint.cpp)
target_link_libraries(test_rustint PRIVATE rustint)
add_test(NAME test_rustint COMMAND test_rustint)
