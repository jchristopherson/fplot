# CMake project file for FPLOT
cmake_minimum_required(VERSION 3.24)
project(
    fplot 
    LANGUAGES Fortran
    VERSION 1.7.0
)

# Get helper macros and functions
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

# Confgiure everything
add_subdirectory(configure)

# Locate Dependencies
add_subdirectory(dependencies)

# Source
add_subdirectory(src)
add_fortran_library(
    ${PROJECT_NAME}
    ${PROJECT_INCLUDE_DIR}
    ${CMAKE_INSTALL_INCLUDEDIR}
    ${PROJECT_VERSION}
    ${PROJECT_VERSION_MAJOR}
    ${FPLOT_SOURCES}
)
link_library(${PROJECT_NAME} ${ferror_LIBRARY} ${ferror_INCLUDE_DIR})
link_library(${PROJECT_NAME} ${iso_varying_string_LIBRARY} ${iso_varying_string_INCLUDE_DIR})
link_library(${PROJECT_NAME} ${collections_LIBRARY} ${collections_INCLUDE_DIR})
link_library(${PROJECT_NAME} ${geompack_LIBRARY} ${geompack_INCLUDE_DIR})
link_library(${PROJECT_NAME} ${forcolormap_LIBRARY} ${forcolormap_INCLUDE_DIR})

# Installation
# add_subdirectory(install)

# ------------------------------------------------------------------------------
# EXAMPLES
# ------------------------------------------------------------------------------
option(BUILD_FPLOT_EXAMPLES "Build FPLOT examples?" OFF)
if (BUILD_FPLOT_EXAMPLES)
    # Inform the user we're building the examples
    message(STATUS "Building FPLOT examples.")

    # Build the examples
    add_subdirectory(examples)
endif()

# ------------------------------------------------------------------------------
# TESTING
# ------------------------------------------------------------------------------
option(BUILD_TESTING "Build tests")
include(CTest)
message(STATUS "Build tests: ${BUILD_TESTING}")
if (BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()