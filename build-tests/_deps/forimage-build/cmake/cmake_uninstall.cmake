if (NOT EXISTS "D:/Code/fplot/build-tests/_deps/forimage-build/install_manifest.txt")
  message(FATAL_ERROR "Cannot find install manifest: \"D:/Code/fplot/build-tests/_deps/forimage-build/install_manifest.txt\"")
endif()

file(READ "D:/Code/fplot/build-tests/_deps/forimage-build/install_manifest.txt" files)
string(REGEX REPLACE "\n" ";" files "${files}")

foreach (file ${files})
  message(STATUS "Removing file: ${file}")
  if (EXISTS "${file}")
    file(REMOVE "${file}")
  endif()
endforeach()
