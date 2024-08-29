cmake_minimum_required(VERSION 3.22)

project(mfront_plaxis_interface
  LANGUAGES CXX;Fortran)
enable_language(Fortran)

set(CMAKE_CXX_STANDARD 20)
set(CXX_STANDARD_REQUIRED ON)

include(cmake/dependencies.cmake)

add_library(MGISPlaxisInterface SHARED
  User_Mod.f90
  MGISPlaxisInterface.cxx)
target_link_libraries(MGISPlaxisInterface
  PRIVATE MFrontGenericInterface)

add_executable(test test.f90)
target_link_libraries(test PRIVATE MGISPlaxisInterface)
