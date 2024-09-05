# Compiling with intel compilers

mkdir build
cmake ../ -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=icpx -DCMAKE_Fortran_COMPILER=ifort -G Ninja ..\plaxis-udm -DCMAKE_INSTALL_PREFIX=c:\Users\th202608\codes\plaxis-udm\master\install
cmake --build . --target all
cmake --build . --target install
