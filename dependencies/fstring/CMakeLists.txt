# Get the macros and functions we'll need
include(FetchContent)

# Fetch the proper content
FetchContent_Declare(
    fstring
    GIT_REPOSITORY "https://github.com/jchristopherson/fstring"
    GIT_TAG main
)

FetchContent_MakeAvailable(fstring)

set(fstring_INCLUDE_DIR ${fstring_BINARY_DIR}/include)
set(fstring_INCLUDE_DIR ${fstring_INCLUDE_DIR} PARENT_SCOPE)

# Make a parent-scope variable for the library
set(fstring_LIBRARY fstring)
set(fstring_LIBRARY ${fstring_LIBRARY} PARENT_SCOPE)
