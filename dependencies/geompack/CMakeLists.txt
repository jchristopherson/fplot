# Get the macros and functions we'll need
include(FetchContent)

# Fetch the proper content
FetchContent_Declare(
    geompack
    GIT_REPOSITORY "https://github.com/jchristopherson/geompack"
    GIT_TAG main
)

FetchContent_MakeAvailable(geompack)

set(geompack_INCLUDE_DIR ${geompack_BINARY_DIR}/include)
set(geompack_INCLUDE_DIR ${geompack_INCLUDE_DIR} PARENT_SCOPE)

# Make a parent-scope variable for the library
set(geompack_LIBRARY geompack)
set(geompack_LIBRARY ${geompack_LIBRARY} PARENT_SCOPE)
