cmake_minimum_required(VERSION 3.28)
project(eva-llvm)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_COMPILER "clang++")

include_directories(/opt/homebrew/Cellar/llvm/18.1.5/include)
include_directories(${CMAKE_BINARY_DIR})
include_directories(${CMAKE_SOURCE_DIR})
include_directories(/opt/homebrew/opt/bdw-gc/include)

# Add custom commands to run Bison and Flex without interference
add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/evaGrammar.cpp
        COMMAND bison -d -o ${CMAKE_BINARY_DIR}/evaGrammar.cpp --defines=${CMAKE_BINARY_DIR}/evaGrammar.h ${CMAKE_SOURCE_DIR}/evaGrammar.y
        MAIN_DEPENDENCY ${CMAKE_SOURCE_DIR}/evaGrammar.y
)

add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/evaLexer.cpp
        COMMAND flex --outfile ${CMAKE_BINARY_DIR}/evaLexer.cpp --header-file=${CMAKE_BINARY_DIR}/evaLexer.h ${CMAKE_SOURCE_DIR}/evaLex.l
        MAIN_DEPENDENCY ${CMAKE_SOURCE_DIR}/evaLex.l
)

# Add custom target to ensure Flex and Bison run
add_custom_target(generate DEPENDS ${CMAKE_BINARY_DIR}/evaGrammar.cpp ${CMAKE_BINARY_DIR}/evaLexer.cpp)

add_executable(eva-llvm
        evaParser.cpp
        evaLLM.cpp
        ${CMAKE_BINARY_DIR}/evaGrammar.cpp
        ${CMAKE_BINARY_DIR}/evaLexer.cpp
        evaEnvironment.h
        evaUtilities.h
)
target_compile_options(eva-llvm PRIVATE
        -D__STDC_CONSTANT_MACROS
        -D__STDC_FORMAT_MACROS
        -D__STDC_LIMIT_MACROS
        -std=c++20
        -stdlib=libc++
        -O0
)
target_link_libraries(eva-llvm PRIVATE
        -L/opt/homebrew/Cellar/llvm/18.1.5/lib
        -L/opt/homebrew/opt/bdw-gc/lib
        -Wl,-search_paths_first
        -Wl,-headerpad_max_install_names
        -lLLVM-18
        -ll
        /opt/homebrew/opt/bdw-gc/lib/libgc.a
        -g
)

target_link_libraries(eva-llvm PRIVATE)
set_target_properties(eva-llvm PROPERTIES LINKER_LANGUAGE CXX)

# Add dependency on the custom generate target
add_dependencies(eva-llvm generate)

# Add clean option
add_custom_target(clean-all
        COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_BINARY_DIR}/CMakeFiles
        COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_BINARY_DIR}/cmake_install.cmake
        COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_BINARY_DIR}/evaGrammar.cpp
        COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_BINARY_DIR}/evaGrammar.h
        COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_BINARY_DIR}/evaLexer.cpp
        COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_BINARY_DIR}/evaLexer.h
        COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_BINARY_DIR}/eva-llvm
        COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_BINARY_DIR}/eva.ll
        COMMENT "Cleaning up all build files"
)
