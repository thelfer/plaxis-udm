program main
  ! integer r
  ! call User_Mod(1, 2, r)
  integer n
  call GetModelCount(n) 
  write(*,*) "number of user defined models: ", n
end program main
