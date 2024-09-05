program main
  ! integer r
  ! call User_Mod(1, 2, r)
  integer n
  integer nprops
  integer nstatev
  character(len=255) name
  call GetModelCountImplementation(n)
  write(*,*) "Number of user defined models: ", n
  write(*,*)
  do i=1, n
     call GetModelNameImplementation(i, name)
     write(*,*) "Description of model (", i, ")", ": '", name, "'"
     write(*,*) ""
     call GetParamCountImplementation(i, nprops)
     if (nprops.ne.0) then
        write(*,*) "List of material properties: "
        do j=1, nprops
           call GetParamName(i, j, name)
           write(*,*) "- ", j, ": ", name
        enddo
     else
        write(*,*) "No material property defined"
     endif
     call GetStateVarCountImplementation(i, nstatev)
     write(*,*) ""
     if (nstatev.ne.0) then
        write(*,*) "List of internal state variables: "
        do j=1, nstatev
           call GetStateVarNameImplementation(i, j, name)
           write(*,*) "- ", j, ": ", name
        enddo
     else
        write(*,*) "No internal state variable defined"
     endif
  enddo
end program main
