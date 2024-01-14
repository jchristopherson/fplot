# Get the macros and functions we'll need
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")
include(FetchContent)

# Get FERROR
add_subdirectory(ferror)
set(ferror_LIBRARY ${ferror_LIBRARY} PARENT_SCOPE)
set(ferror_INCLUDE_DIR ${ferror_INCLUDE_DIR} PARENT_SCOPE)

# Get FSTRING
add_subdirectory(fstring)
set(fstring_LIBRARY ${fstring_LIBRARY} PARENT_SCOPE)
set(fstring_INCLUDE_DIR ${fstring_INCLUDE_DIR} PARENT_SCOPE)

# Get COLLECTIONS
add_subdirectory(collections)
set(collections_LIBRARY ${collections_LIBRARY} PARENT_SCOPE)
set(collections_INCLUDE_DIR ${collections_INCLUDE_DIR} PARENT_SCOPE)

# Get GEOMPACK
add_subdirectory(geompack)
set(geompack_LIBRARY ${geompack_LIBRARY} PARENT_SCOPE)
set(geompack_INCLUDE_DIR ${geompack_INCLUDE_DIR} PARENT_SCOPE)

# Get FORCOLORMAP
add_subdirectory(forcolormap)
set(forcolormap_LIBRARY ${forcolormap_LIBRARY} PARENT_SCOPE)
set(forcolormap_INCLUDE_DIR ${forcolormap_INCLUDE_DIR} PARENT_SCOPE)
