# CMake project file for FPLOT
cmake_minimum_required(VERSION 3.0)
project(fplot Fortran)

# Define version information
set(FPLOT_MAJOR_VERSION 1)
set(FPLOT_MINOR_VERSION 2)
set(FPLOT_PATCH_VERSION 0)
set(FPLOT_VERSION ${FPLOT_MAJOR_VERSION},${FPLOT_MINOR_VERSION},${FPLOT_PATCH_VERSION})

# Locate Dependencies
find_package(ferror 1.3.0)
find_package(collections)
set(strings_Sources ${PROJECT_SOURCE_DIR}/external/strings/src/strings.f90)

# If FERROR is not installed on the system, build the default implementation
if (NOT ${ferror_FOUND})
    message(STATUS "FERROR not found.  The default implementation will be used.")
    add_subdirectory(external/ferror)
    include_directories(external/ferror/include)
    set(FERROR_LIBRARIES ferror)
    set(ferror_LibLocation ${ferror_BINARY_DIR})
else()
    message(STATUS "An acceptable version of FERROR (v" ${ferror_VERSION} ") was found, and will be utilized.")
    include_directories(${ferror_INCLUDE_DIRS})
    set(FERROR_LIBRARIES ferror)
    get_target_property(ferror_LibLocation ferror LOCATION)
endif()

# If COLLECTIONS is not installed on the system, build the default implementation
if (NOT ${collections_FOUND})
    message(STATUS "COLLECTIONS not found.  The default implementation will be used.")
    add_subdirectory(external/collections)
    include_directories(external/collections/include)
    set(COLLECTIONS_LIBRARIES collections)
    set(collections_LibLocation ${collections_BINARY_DIR})
else()
    message(STATUS "An acceptable version of COLLECTIONS (v" ${collections_VERSION} ") was found, and will be utilized.")
    set(COLLECTIONS_LIBRARIES collections)
    get_target_property(collections_LibLocation collections LOCATION)
endif()

# Export all symbols on Windows when building shared libraries
SET(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

# Locate the module files
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/include)

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
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -Wall -Wno-surprising")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -Wall")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

# Define sources
add_subdirectory(src)

# ------------------------------------------------------------------------------
# EXAMPLES
# ------------------------------------------------------------------------------
add_subdirectory(examples)