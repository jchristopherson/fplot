# CMake project file for FPLOT
cmake_minimum_required(VERSION 3.24)
project(
    fplot 
    LANGUAGES Fortran
    VERSION 1.8.2
)

# Confgiure everything
add_subdirectory(configure)

# Source
add_subdirectory(src)

# Install
install(
  EXPORT ${PROJECT_NAME}-targets
  NAMESPACE ${PROJECT_NAME}::
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)

# Testing
option(BUILD_TESTING "Build tests")
include(CTest)
message(STATUS "Build FPLOT tests: ${BUILD_TESTING}")
if (BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()

# Examples
option(BUILD_FPLOT_EXAMPLES "Build FPLOT examples")
message(STATUS "Build FPLOT examples: ${BUILD_FPLOT_EXAMPLES}")
if (BUILD_FPLOT_EXAMPLES)
    add_subdirectory(examples)
endif()