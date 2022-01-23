cmake_minimum_required(VERSION 2.8)
project(esplugin-ffi-tests CXX C)

include_directories("${CMAKE_SOURCE_DIR}/include")

if (CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
    set (SYSTEM_LIBS pthread dl)
endif ()

if (MSVC)
    set (SYSTEM_LIBS ws2_32 Userenv bcrypt)
endif ()

add_executable(ffi_cpp_tests "${CMAKE_SOURCE_DIR}/tests/ffi.cpp")
target_link_libraries(ffi_cpp_tests "${CMAKE_SOURCE_DIR}/../target/debug/${CMAKE_STATIC_LIBRARY_PREFIX}esplugin_ffi${CMAKE_STATIC_LIBRARY_SUFFIX}" ${SYSTEM_LIBS})

enable_testing()
add_test(ffi_cpp_tests ffi_cpp_tests)
