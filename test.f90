program main
  ! integer r
  ! call User_Mod(1, 2, r)
  integer n
  integer nprops
  integer nstatev
  character(len=255) name
  call GetModelCount(n)
  write(*,*) "Number of user defined models: ", n
  write(*,*)
  do i=1, n
     call GetModelName(i, name)
     write(*,*) i, "Description of model ", ": '", name, "'"
     write(*,*) ""
     call GetParamCount(i, nprops)
     write(*,*) "List of material properties: "
     do j=1, nprops
        call GetParamName(i, j, name)
        write(*,*) "- ", j, ": ", name
     enddo
     write(*,*) ""
     write(*,*) "List of internal state variables: "
     call GetStateVarCount(i, nstatev)
     do j=1, nstatev
        call GetStateVarName(i, j, name)
        write(*,*) "- ", j, ": ", name
     enddo
  enddo
end program main
