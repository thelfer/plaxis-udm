subroutine User_Mod(IDTask, iMod, IsUndr, iStep, iTer, Iel,Int, X, &
     Y, Z, Time0, dTime, Props, Sig0, Swp0, StVar0, &
     dEps, D, Bulk_W, Sig, Swp, StVar, ipl, nStat, &
     NonSym, iStrsDep, iTimeDep, iTang, iPrjDir, &
     iPrjLen, iAbort)
  implicit none
  interface
     function mgis_plaxis_interface_wrapper(task, model, &
          nisvs, unsymmetric, stressDependent, timeDependent, constistentTangentOperator, &
          D, Sig, StVar, & 
          Props, Sig0, StVar0, dEps, T, dt) &
          bind(c,name = 'mgis_plaxis_interface') &
          result(r)
       use, intrinsic :: iso_c_binding, only: c_int, c_double
       implicit none
       integer(kind=c_int), intent(in), value :: task
       integer(kind=c_int), intent(in), value :: model
       integer(kind=c_int), intent(out) :: nisvs
       integer(kind=c_int), intent(out) :: unsymmetric
       integer(kind=c_int), intent(out) :: stressDependent
       integer(kind=c_int), intent(out) :: timeDependent
       integer(kind=c_int), intent(out) :: constistentTangentOperator
       real(kind=c_double), intent(out) :: D(6,6)
       real(kind=c_double), intent(out) :: Sig(6)
       real(kind=c_double), intent(out) :: StVar(*)
       real(kind=c_double), intent(in) :: Props(*)
       real(kind=c_double), intent(in) :: Sig0(6)
       real(kind=c_double), intent(in) :: StVar0(*)
       real(kind=c_double), intent(in) :: dEps(12)
       real(kind=c_double), intent(in), value :: T
       real(kind=c_double), intent(in), value :: dt
       integer(kind=c_int) :: r
     end function mgis_plaxis_interface_wrapper
  end interface
  integer, intent(out) :: nStat, NonSym, iStrsDep, iTimeDep, iTang
  double precision, intent(out) :: D (6, 6)
  double precision, intent(out) :: Sig (6), StVar(*)
  integer, intent(in) :: IsUndr, iStep, iTer, Iel, Int, ipl
  integer, intent(in) :: iPrjDir, iPrjLen
  double precision, intent(in) :: X, Y, Z, Bulk_W, Swp0, Time0, Swp
  double precision, intent(in) :: dTime
  double precision, intent(in) :: Props(*)
  double precision, intent(in) :: Sig0(20), StVar0(*)
  double precision, intent(in) :: dEps(12)
  integer IDTask, iMod, iAbort
  double precision T
  integer dummy_integer
  double precision dummy_real
  T = Sig0(18)
  iAbort = mgis_plaxis_interface_wrapper(IDTask, iMod, &
       nStat, NonSym, iStrsDep, iTimeDep, iTang,       &
       D, Sig, StVar,                                  &
       Props , Sig0, StVar0, dEps, T, dTime)
  ! removing unused variable warning
  if (.false.) dummy_integer = IsUndr
  if (.false.) dummy_integer = Iel
  if (.false.) dummy_integer = iStep
  if (.false.) dummy_integer = iTer
  if (.false.) dummy_integer = Int
  if (.false.) dummy_integer = ipl
  if (.false.) dummy_integer = iPrjDir
  if (.false.) dummy_integer = iPrjLen
  if (.false.) dummy_real = X
  if (.false.) dummy_real = Y
  if (.false.) dummy_real = Z
  if (.false.) dummy_real = Bulk_w
  if (.false.) dummy_real = Swp0
  if (.false.) dummy_real = Time0
  if (.false.) dummy_real = Swp
end subroutine User_Mod

subroutine GetModelCount (C)
  implicit none
  interface
     function get_model_count_wrapper() &
          bind(c,name = 'mgis_plaxis_interface_get_model_count') &
          result(r)
       use, intrinsic :: iso_c_binding, only: c_int
       implicit none
       integer(kind=c_int) :: r
     end function get_model_count_wrapper
  end interface
  integer (kind=4) , intent(out) :: C
!DEC$ ATTRIBUTES DLLExport, StdCall, reference ::  GetModelCount
  C = get_model_count_wrapper()
end subroutine GetModelCount

subroutine GetModelName(iModel, name)
  implicit none
  interface
     subroutine get_model_name_wrapper(name, model) &
          bind(c,name = 'mgis_plaxis_interface_get_model_name') 
       use, intrinsic :: iso_c_binding, only: c_int, c_char
       implicit none
       character(kind=c_char), dimension(*) :: name
       integer(kind=c_int), intent(in), value :: model
     end subroutine get_model_name_wrapper
  end interface
  integer, intent(in) :: iModel
  character(len= *), intent(out) :: name
  character (len=255) tmp
!DEC$ ATTRIBUTES DLLExport, StdCall, reference ::  GetModelName
  call get_model_name_wrapper(tmp, iModel)
  name = tmp(:)
end subroutine GetModelName

subroutine GetParamCount (iModel, C)
  implicit none
  interface
     function get_material_properties_count_wrapper(model) &
          bind(c,name = 'mgis_plaxis_interface_get_material_properties_count') &
          result(r)
       use, intrinsic :: iso_c_binding, only: c_int
       implicit none
       integer(kind=c_int), intent(in), value :: model
       integer(kind=c_int) :: r
     end function get_material_properties_count_wrapper
  end interface
  integer, intent(in)  :: iModel
  integer, intent(out) :: C
!DEC$ ATTRIBUTES DLLExport, StdCall, reference ::  GetParamCount
  C = get_material_properties_count_wrapper(iModel)
end subroutine GetParamCount

subroutine GetParamName (iModel, iParam, name)
  implicit none
  interface
     subroutine get_material_property_name_wrapper(name, model, i) &
          bind(c,name = 'mgis_plaxis_interface_get_material_property_name')
       use, intrinsic :: iso_c_binding, only: c_int, c_char
       implicit none
       character(kind=c_char), dimension(*) :: name
       integer(kind=c_int), intent(in), value :: model
       integer(kind=c_int), intent(in), value :: i
     end subroutine get_material_property_name_wrapper
  end interface
  integer, intent(in)  :: iModel
  integer, intent(in)  :: iParam
  character(len= *), intent(out) :: name
!DEC$ ATTRIBUTES DLLExport, StdCall, reference ::  GetParamName
  character (len=255) tmp
  call get_material_property_name_wrapper(tmp, iModel, iParam)
  name = tmp(:)
end subroutine GetParamName

subroutine GetStateVarCount (iModel, C)
  implicit none
  interface
     function get_internal_state_variables_count_wrapper(model) &
          bind(c,name = 'mgis_plaxis_interface_get_internal_state_variables_count') &
          result(r)
       use, intrinsic :: iso_c_binding, only: c_int
       implicit none
       integer(kind=c_int), intent(in), value :: model
       integer(kind=c_int) :: r
     end function get_internal_state_variables_count_wrapper
  end interface
  integer, intent(in)  :: iModel
  integer, intent(out) :: C
!DEC$ ATTRIBUTES DLLExport, StdCall, reference ::  GetStateVarCount
  C = get_internal_state_variables_count_wrapper(iModel)
end subroutine GetStateVarCount

subroutine GetStateVarName (iModel, iStateVar, name)
  implicit none
  interface
     subroutine get_state_variable_name_wrapper(name, model, i) &
          bind(c,name = 'mgis_plaxis_interface_get_state_variable_name')
       use, intrinsic :: iso_c_binding, only: c_int, c_char
       implicit none
       character(kind=c_char), dimension(*) :: name
       integer(kind=c_int), intent(in), value :: model
       integer(kind=c_int), intent(in), value :: i
     end subroutine get_state_variable_name_wrapper
  end interface
  integer, intent(in)  :: iModel
  integer, intent(in)  :: iStateVar
  character(len= *), intent(out) :: name
!DEC$ ATTRIBUTES DLLExport, StdCall, reference ::  GetStateVarName
  character (len=255) tmp
  call get_state_variable_name_wrapper(tmp, iModel, iStateVar)
  name = tmp(:)
end subroutine GetStateVarName
