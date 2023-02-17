# Get the macros and functions we'll need
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")
include(FetchContent)

# Fetch the proper content
FetchContent_Declare(
    collections
    GIT_REPOSITORY "https://github.com/jchristopherson/collections"
    GIT_TAG main
)

FetchContent_MakeAvailable(collections)

if (WIN32)
    if (BUILD_SHARED_LIBS)
        add_custom_command(
            TARGET ${PROJECT_NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_FILE:collections>
            $<TARGET_FILE_DIR:${PROJECT_NAME}>
        )
    endif()
endif()

set(collections_INCLUDE_DIR ${collections_BINARY_DIR}/include)
set(collections_INCLUDE_DIR ${collections_INCLUDE_DIR} PARENT_SCOPE)

# Make a parent-scope variable for the library
set(collections_LIBRARY collections)
set(collections_LIBRARY ${collections_LIBRARY} PARENT_SCOPE)
