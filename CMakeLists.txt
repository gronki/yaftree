cmake_minimum_required(VERSION 3.16)
project(yaftree LANGUAGES Fortran)
enable_testing()

file(GLOB SRC_SOURCES CONFIGURE_DEPENDS
    src/*.f90
    src/*.F90
    src/*.c
)

add_library(yaftree_static STATIC ${SRC_SOURCES})

# Where to put .mod files
set_target_properties(yaftree_static PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod
)

target_include_directories(yaftree_static PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}/mod
)

# Example executable
add_executable(yaftree_demo example/demo.f90)
target_link_libraries(yaftree_demo PRIVATE yaftree_static)

file(GLOB TEST_SOURCES CONFIGURE_DEPENDS test/*.f90)

foreach(test_src ${TEST_SOURCES})
    get_filename_component(test_name ${test_src} NAME_WE)
    add_executable(${test_name} ${test_src})
    target_link_libraries(${test_name} PRIVATE yaftree_static)
    add_test(NAME ${test_name} COMMAND ${CMAKE_SOURCE_DIR}/get-test-data.sh && ${test_name})
endforeach()
