subroutine User_Mod(IDTask, iMod, iAbort)
  ! , IsUndr, iStep, iTer, Iel,Int, X, &
  ! Y, Z, Time0, dTime, Props, Sig0, Swp0, StVar0, &
  ! dEps, D, Bulk_W, Sig, Swp, StVar, ipl, nStat, &
  ! NonSym, iStrsDep, iTimeDep, iTang, iPrjDir, &
  ! iPrjLen, iAbort)
  implicit none
  interface
     function mfront_plaxis_interface_wrapper(task, mode) &
          bind(c,name = 'mfront_plaxis_interface') &
          result(r)
       use, intrinsic :: iso_c_binding, only: c_int
       implicit none
       integer(kind=c_int), intent(in), value :: task
       integer(kind=c_int), intent(in), value :: mode
       integer(kind=c_int) :: r
     end function mfront_plaxis_interface_wrapper
  end interface
  integer IDTask
  integer iMod
  integer iAbort
  iAbort = mfront_plaxis_interface_wrapper(IDTask, iMod)
end subroutine User_Mod


program main
  integer r
  call  User_Mod(1, 2, r)
end program main
