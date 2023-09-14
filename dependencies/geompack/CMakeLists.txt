# Get the macros and functions we'll need
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")
include(FetchContent)

# Fetch the proper content
FetchContent_Declare(
    geompack
    GIT_REPOSITORY "https://github.com/jchristopherson/geompack"
    GIT_TAG main
)

FetchContent_MakeAvailable(geompack)

if (WIN32)
    if (BUILD_SHARED_LIBS)
        add_custom_command(
            TARGET ${PROJECT_NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_FILE:geompack>
            $<TARGET_FILE_DIR:${PROJECT_NAME}>
        )
    endif()
endif()

set(geompack_INCLUDE_DIR ${geompack_BINARY_DIR}/include)
set(geompack_INCLUDE_DIR ${geompack_INCLUDE_DIR} PARENT_SCOPE)

# Make a parent-scope variable for the library
set(geompack_LIBRARY geompack)
set(geompack_LIBRARY ${geompack_LIBRARY} PARENT_SCOPE)
