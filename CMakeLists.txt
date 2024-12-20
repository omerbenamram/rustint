cmake_minimum_required(VERSION 3.27)
project(rustint)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_library(rustint INTERFACE)
target_include_directories(
  rustint INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
                    $<INSTALL_INTERFACE:include>)

add_executable(test_rustint tests/test_rustint.cpp)
target_link_libraries(test_rustint rustint)
