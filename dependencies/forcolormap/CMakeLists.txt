# Get the macros and functions we'll need
include(FetchContent)

# Fetch the proper content
FetchContent_Declare(
    forcolormap
    GIT_REPOSITORY "https://github.com/vmagnin/forcolormap"
    GIT_TAG main
)

FetchContent_MakeAvailable(forcolormap)

set(forcolormap_INCLUDE_DIR ${forcolormap_BINARY_DIR}/include)
set(forcolormap_INCLUDE_DIR ${forcolormap_INCLUDE_DIR} PARENT_SCOPE)

# Make a parent-scope variable for the library
set(forcolormap_LIBRARY forcolormap)
set(forcolormap_LIBRARY ${forcolormap_LIBRARY} PARENT_SCOPE)
