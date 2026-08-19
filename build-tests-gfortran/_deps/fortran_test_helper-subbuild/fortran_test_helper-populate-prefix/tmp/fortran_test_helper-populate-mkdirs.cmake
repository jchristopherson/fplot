# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.5)

file(MAKE_DIRECTORY
  "D:/Code/fplot/build-tests-gfortran/_deps/fortran_test_helper-src"
  "D:/Code/fplot/build-tests-gfortran/_deps/fortran_test_helper-build"
  "D:/Code/fplot/build-tests-gfortran/_deps/fortran_test_helper-subbuild/fortran_test_helper-populate-prefix"
  "D:/Code/fplot/build-tests-gfortran/_deps/fortran_test_helper-subbuild/fortran_test_helper-populate-prefix/tmp"
  "D:/Code/fplot/build-tests-gfortran/_deps/fortran_test_helper-subbuild/fortran_test_helper-populate-prefix/src/fortran_test_helper-populate-stamp"
  "D:/Code/fplot/build-tests-gfortran/_deps/fortran_test_helper-subbuild/fortran_test_helper-populate-prefix/src"
  "D:/Code/fplot/build-tests-gfortran/_deps/fortran_test_helper-subbuild/fortran_test_helper-populate-prefix/src/fortran_test_helper-populate-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "D:/Code/fplot/build-tests-gfortran/_deps/fortran_test_helper-subbuild/fortran_test_helper-populate-prefix/src/fortran_test_helper-populate-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "D:/Code/fplot/build-tests-gfortran/_deps/fortran_test_helper-subbuild/fortran_test_helper-populate-prefix/src/fortran_test_helper-populate-stamp${cfgdir}") # cfgdir has leading slash
endif()
