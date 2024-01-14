# Get the macros and functions we'll need
include(FetchContent)

# Fetch the proper content
FetchContent_Declare(
    fstring
    GIT_REPOSITORY "https://github.com/jchristopherson/fstring"
    GIT_TAG main
)

FetchContent_MakeAvailable(fstring)

if (WIN32)
    if (BUILD_SHARED_LIBS)
        add_custom_command(
            TARGET ${PROJECT_NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_FILE:fstring>
            $<TARGET_FILE_DIR:${PROJECT_NAME}>
        )
    endif()
endif()

set(fstring_INCLUDE_DIR ${fstring_BINARY_DIR}/include)
set(fstring_INCLUDE_DIR ${fstring_INCLUDE_DIR} PARENT_SCOPE)

# Make a parent-scope variable for the library
set(fstring_LIBRARY fstring)
set(fstring_LIBRARY ${fstring_LIBRARY} PARENT_SCOPE)
