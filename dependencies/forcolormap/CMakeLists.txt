# Get the macros and functions we'll need
include(FetchContent)

# Fetch the proper content
FetchContent_Declare(
    forcolormap
    GIT_REPOSITORY "https://github.com/jchristopherson/forcolormap"
    GIT_TAG main
)

FetchContent_MakeAvailable(forcolormap)

if (WIN32)
    if (BUILD_SHARED_LIBS)
        add_custom_command(
            TARGET ${PROJECT_NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_FILE:forcolormap>
            $<TARGET_FILE_DIR:${PROJECT_NAME}>
        )
    endif()
endif()

set(forcolormap_INCLUDE_DIR ${forcolormap_BINARY_DIR}/include)
set(forcolormap_INCLUDE_DIR ${forcolormap_INCLUDE_DIR} PARENT_SCOPE)

# Make a parent-scope variable for the library
set(forcolormap_LIBRARY forcolormap)
set(forcolormap_LIBRARY ${forcolormap_LIBRARY} PARENT_SCOPE)
