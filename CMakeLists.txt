# CMake project file for FPLOT
cmake_minimum_required(VERSION 3.7)
project(fplot Fortran C)

# Define version information
set(FPLOT_MAJOR_VERSION 1)
set(FPLOT_MINOR_VERSION 5)
set(FPLOT_PATCH_VERSION 0)
set(FPLOT_VERSION ${FPLOT_MAJOR_VERSION}.${FPLOT_MINOR_VERSION}.${FPLOT_PATCH_VERSION})

# Locate Dependencies
find_package(ferror 1.3.0)
find_package(fcore 1.0.0)

if (NOT ${ferror_FOUND})
    message(STATUS "FERROR not found.")
else()
    message(STATUS "An acceptable version of FERROR (v" ${ferror_VERSION} ") was found, and will be utilized.")
    include_directories(${ferror_INCLUDE_DIRS})
    set(FERROR_LIBRARIES ferror)
    get_target_property(ferror_LibLocation ferror LOCATION)
endif()

if (NOT ${fcore_FOUND})
    message(STATUS "FCORE not found.")
else()
    message(STATUS "An acceptable version of FCORE (v" ${fcore_VERSION} ") was found, and will be utilized.")
    include_directories(${fcore_INCLUDE_DIRS})
    set(FCORE_LIBRARIES fcore)
    get_target_property(fcore_LibLocation fcore LOCATION)
endif()

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
endif()

# By default, shared library
option(BUILD_SHARED_LIBS "Build shared libraries" ON)

# Export all symbols on Windows when building shared libraries
SET(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

# Locate the module files
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/include)

# Locate header files
include_directories(${PROJECT_SOURCE_DIR}/include)

# Define output directories, if undefined
if (NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
    message(STATUS "FPLOT library output directories undefined.  Using default directories.")
    if (CMAKE_BUILD_TYPE MATCHES Debug)
        # Debug Build
        if (BUILD_SHARED_LIBS)
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
        else()
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
        endif()
    elseif (CMAKE_BUILD_TYPE MATCHES Release)
        # Release Build
        if (BUILD_SHARED_LIBS)
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
        else()
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
        endif()
    else()
        # Default Condition
        if (BUILD_SHARED_LIBS)
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
        else()
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
        endif()
    endif()
endif()

# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  # Options: https://gcc.gnu.org/onlinedocs/gfortran/Option-Summary.html
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -Wl,--allow-multiple-definition")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -Wall -Wno-surprising -Wl,--allow-multiple-definition")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # Assume gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -Wl,--allow-multiple-definition")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -Wall -Wno-surprising -Wl,--allow-multiple-definition")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

# Define sources
add_subdirectory(src)

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
