# Get the macros and functions we'll need
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")
include(FetchContent)

# Fetch the proper content
set(INSTALL_PROJECT OFF CACHE INTERNAL "")
FetchContent_Declare(
    iso_varying_string
    GIT_REPOSITORY "https://gitlab.com/everythingfunctional/iso_varying_string"
    GIT_TAG main
)

FetchContent_MakeAvailable(iso_varying_string)

set(iso_varying_string_INCLUDE_DIR ${iso_varying_string_BINARY_DIR}/src/include)
set(iso_varying_string_INCLUDE_DIR ${iso_varying_string_INCLUDE_DIR} PARENT_SCOPE)

# Make a parent-scope variable for the library
set(iso_varying_string_LIBRARY iso_varying_string)
set(iso_varying_string_LIBRARY ${iso_varying_string_LIBRARY} PARENT_SCOPE)

