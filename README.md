# Compiling with intel compilers

mkdir build
cmake ../ -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=icpx -DCMAKE_Fortran_COMPILER=ifx
cmake --build . --target all