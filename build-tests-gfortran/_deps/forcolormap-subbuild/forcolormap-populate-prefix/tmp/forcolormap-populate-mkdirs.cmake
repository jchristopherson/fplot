# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.5)

file(MAKE_DIRECTORY
  "D:/Code/fplot/build-tests-gfortran/_deps/forcolormap-src"
  "D:/Code/fplot/build-tests-gfortran/_deps/forcolormap-build"
  "D:/Code/fplot/build-tests-gfortran/_deps/forcolormap-subbuild/forcolormap-populate-prefix"
  "D:/Code/fplot/build-tests-gfortran/_deps/forcolormap-subbuild/forcolormap-populate-prefix/tmp"
  "D:/Code/fplot/build-tests-gfortran/_deps/forcolormap-subbuild/forcolormap-populate-prefix/src/forcolormap-populate-stamp"
  "D:/Code/fplot/build-tests-gfortran/_deps/forcolormap-subbuild/forcolormap-populate-prefix/src"
  "D:/Code/fplot/build-tests-gfortran/_deps/forcolormap-subbuild/forcolormap-populate-prefix/src/forcolormap-populate-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "D:/Code/fplot/build-tests-gfortran/_deps/forcolormap-subbuild/forcolormap-populate-prefix/src/forcolormap-populate-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "D:/Code/fplot/build-tests-gfortran/_deps/forcolormap-subbuild/forcolormap-populate-prefix/src/forcolormap-populate-stamp${cfgdir}") # cfgdir has leading slash
endif()
