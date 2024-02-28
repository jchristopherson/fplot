# Get the macros and functions we'll need
include(FetchContent)

# Fetch the proper content
FetchContent_Declare(
    collections
    GIT_REPOSITORY "https://github.com/jchristopherson/collections"
    GIT_TAG main
)

FetchContent_MakeAvailable(collections)

set(collections_INCLUDE_DIR ${collections_BINARY_DIR}/include)
set(collections_INCLUDE_DIR ${collections_INCLUDE_DIR} PARENT_SCOPE)

# Make a parent-scope variable for the library
set(collections_LIBRARY collections)
set(collections_LIBRARY ${collections_LIBRARY} PARENT_SCOPE)
