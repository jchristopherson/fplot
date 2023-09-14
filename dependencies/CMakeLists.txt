# Get the macros and functions we'll need
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")
include(FetchContent)

# Get FERROR
add_subdirectory(ferror)
set(ferror_LIBRARY ${ferror_LIBRARY} PARENT_SCOPE)
set(ferror_INCLUDE_DIR ${ferror_INCLUDE_DIR} PARENT_SCOPE)

# Get ISO_VARYING_STRING
add_subdirectory(iso_varying_string)
set(iso_varying_string_LIBRARY ${iso_varying_string_LIBRARY} PARENT_SCOPE)
set(iso_varying_string_INCLUDE_DIR ${iso_varying_string_INCLUDE_DIR} PARENT_SCOPE)

# Get COLLECTIONS
add_subdirectory(collections)
set(collections_LIBRARY ${collections_LIBRARY} PARENT_SCOPE)
set(collections_INCLUDE_DIR ${collections_INCLUDE_DIR} PARENT_SCOPE)

# Get GEOMPACK
add_subdirectory(geompack)
set(geompack_LIBRARY ${geompack_LIBRARY} PARENT_SCOPE)
set(geompack_INCLUDE_DIR ${geompack_INCLUDE_DIR} PARENT_SCOPE)
