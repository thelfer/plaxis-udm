subroutine User_Mod(IDTask, iMod, iAbort)
  ! , IsUndr, iStep, iTer, Iel,Int, X, &
  ! Y, Z, Time0, dTime, Props, Sig0, Swp0, StVar0, &
  ! dEps, D, Bulk_W, Sig, Swp, StVar, ipl, nStat, &
  ! NonSym, iStrsDep, iTimeDep, iTang, iPrjDir, &
  ! iPrjLen, iAbort)
  implicit none
  interface
     function mfront_plaxis_interface_wrapper(task, model) &
          bind(c,name = 'mfront_plaxis_interface') &
          result(r)
       use, intrinsic :: iso_c_binding, only: c_int
       implicit none
       integer(kind=c_int), intent(in), value :: task
       integer(kind=c_int), intent(in), value :: model
       integer(kind=c_int) :: r
     end function mfront_plaxis_interface_wrapper
  end interface
  integer IDTask
  integer iMod
  integer iAbort
  iAbort = mfront_plaxis_interface_wrapper(IDTask, iMod)
end subroutine User_Mod

subroutine GetModelCount(C)
  implicit none
  interface
     function mfront_plaxis_interface_get_model_count_wrapper() &
          bind(c,name = 'mfront_plaxis_interface_get_model_count') &
          result(r)
       use, intrinsic :: iso_c_binding, only: c_int
       implicit none
       integer(kind=c_int) :: r
     end function mfront_plaxis_interface_get_model_count_wrapper
  end interface
  integer, intent(out) :: C
  C = mfront_plaxis_interface_get_model_count_wrapper()
end subroutine GetModelCount

! function GetModelName(iModel, name)
!   implicit none
!   interface
!      subroutine mfront_plaxis_interface_get_model_name_wrapper(name, model) &
!           bind(c,name = 'mfront_plaxis_interface_get_model_name') 
!        use, intrinsic :: iso_c_binding, only: c_int
!        implicit none
!        type(c_ptr), intent(out) :: name
!        integer(kind=c_int), intent(in), value :: model
!      end subroutine mfront_plaxis_interface_get_model_name_wrapper
!   end interface
!   integer iModel
!   character(len=:), allocatable, intent(out) :: name
!   type(c_ptr) :: cname
!   call mfront_plaxis_interface_get_model_name_wrapper(cname, iModel)
!   name = convert_c_string(cname)
! end function GetModelName

! GetParamCount(iModel, C)
! GetParamName(iModel, iParam, Name)
! GetParamUnit(iModel, iParam, Units)
! GetStateVarCount(iModel, C)
! GetStateVarName(iModel, iParam, Name)
! GetStateVarUnit(iModel, iParam, Units)
