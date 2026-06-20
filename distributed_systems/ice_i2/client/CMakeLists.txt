cmake_minimum_required(VERSION 3.16)
project(i2_client CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Ice REQUIRED COMPONENTS Ice)

set(GENERATED_DIR "${CMAKE_SOURCE_DIR}/../generated/cpp")

add_executable(client
    main.cpp
    "${GENERATED_DIR}/Counter.cpp"
)

target_include_directories(client PRIVATE
    "${GENERATED_DIR}"
    ${Ice_INCLUDE_DIRS}
)

target_link_libraries(client PRIVATE
    Ice::Ice
)
