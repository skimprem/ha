!!###############################################################
!! ONE DIM
!!###############################################################
!subroutine nc_variable_conv_byte_one_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=1), dimension(:) :: output_value
  !return
!end subroutine nc_variable_conv_byte_one_dim
!subroutine nc_variable_conv_short_one_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=2), dimension(:) :: output_value
  !return
!end subroutine nc_variable_conv_short_one_dim
!subroutine nc_variable_conv_int_one_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=4), dimension(:) :: output_value
  !return
!end subroutine nc_variable_conv_int_one_dim
!subroutine nc_variable_conv_int64_one_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=8), dimension(:) :: output_value
  !return
!end subroutine nc_variable_conv_int64_one_dim
!subroutine nc_variable_conv_float_one_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !real(kind=4), dimension(:) :: output_value
  !return
!end subroutine nc_variable_conv_float_one_dim
!subroutine nc_variable_conv_double_one_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !real(kind=8), dimension(:) :: output_value
  !return
!end subroutine nc_variable_conv_double_one_dim
!!###############################################################
!! TWO DIM
!!###############################################################
!subroutine nc_variable_conv_byte_two_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=1), dimension(:,:) :: output_value
  !return
!end subroutine nc_variable_conv_byte_two_dim
!subroutine nc_variable_conv_short_two_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=2), dimension(:,:) :: output_value
  !return
!end subroutine nc_variable_conv_short_two_dim
!subroutine nc_variable_conv_int_two_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=4), dimension(:,:) :: output_value
  !return
!end subroutine nc_variable_conv_int_two_dim
!subroutine nc_variable_conv_int64_two_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=8), dimension(:,:) :: output_value
  !return
!end subroutine nc_variable_conv_int64_two_dim
!subroutine nc_variable_conv_float_two_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !real(kind=4), dimension(:,:) :: output_value
  !return
!end subroutine nc_variable_conv_float_two_dim
!subroutine nc_variable_conv_double_two_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !real(kind=8), dimension(:,:) :: output_value
  !!real(kind=8) :: scale_factor
  !!integer(kind=4) :: i
  !!do i = 1, variable%natts
    !!select case(trim(adjustl(variable%attribute(i)%name)))
    !!case('scale_factor')
      !!!scale_factor = value_conv(variable%attribute(i)%value%)
    !!end select
  !!end do
  !return
!end subroutine nc_variable_conv_double_two_dim
!!###############################################################
!! THREE DIM
!!###############################################################
!subroutine nc_variable_conv_byte_three_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=1), dimension(:,:,:) :: output_value
  !return
!end subroutine nc_variable_conv_byte_three_dim
!subroutine nc_variable_conv_short_three_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=2), dimension(:,:,:) :: output_value
  !return
!end subroutine nc_variable_conv_short_three_dim
!subroutine nc_variable_conv_int_three_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=4), dimension(:,:,:) :: output_value
  !return
!end subroutine nc_variable_conv_int_three_dim
!subroutine nc_variable_conv_int64_three_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !integer(kind=8), dimension(:,:,:) :: output_value
  !return
!end subroutine nc_variable_conv_int64_three_dim
!subroutine nc_variable_conv_float_three_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !real(kind=4), dimension(:,:,:) :: output_value
  !return
!end subroutine nc_variable_conv_float_three_dim
!subroutine nc_variable_conv_double_three_dim(variable, output_value)
  !implicit none
  !type(ncvariables) :: variable 
  !real(kind=8), dimension(:,:,:) :: output_value
  !return
!end subroutine nc_variable_conv_double_three_dim

  !!do i = 1, variable%natts
    !!if(trim(adjustl(variable%attribute(i)%name)) == 'scale_factor') then
      !!nc_factor_scale = .true.
      !!select case(variable%attribute(i)%xtype)
      !!case(nf90_byte)
        !!scale_factor = real(variable%attribute(i)%value%byte_1(1), 8)
      !!case(nf90_ubyte)
      !!case(nf90_short)
        !!scale_factor = real(variable%attribute(i)%value%short_1(1), 8)
      !!case(nf90_ushort)
      !!case(nf90_int)
        !!scale_factor = real(variable%attribute(i)%value%int_1(1), 8)
      !!case(nf90_uint)
      !!case(nf90_int64)
        !!scale_factor = real(variable%attribute(i)%value%int64_1(1), 8)
      !!case(nf90_uint64)
      !!case(nf90_float)
        !!scale_factor = real(variable%attribute(i)%value%float_1(1), 8)
      !!case(nf90_double)
        !!scale_factor = real(variable%attribute(i)%value%double_1(1), 8)
      !!case(nf90_char)
      !!case(nf90_string)
      !!end select
      !!select case(variable%xtype)
      !!case(nf90_byte)
        !!select case(variable%ndims)
        !!case(1)
          !!allocate(variable%value%double_1(variable%len(1)))
          !!variable%value%double_1 = real(variable%value%byte_1, 8) * scale_factor 
          !!deallocate(variable%value%byte_1)
        !!case(2)
          !!allocate(variable%value%double_2(variable%len(1), variable%len(2)))
          !!variable%value%double_2 = real(variable%value%byte_2, 8) * scale_factor
          !!deallocate(variable%value%byte_2)
        !!case(3)
          !!allocate(variable%value%double_3(variable%len(1), variable%len(2), variable%len(2)))
          !!variable%value%double_3 = real(variable%value%byte_3, 8) * scale_factor
          !!deallocate(variable%value%byte_3)
        !!end select
      !!case(nf90_ubyte)
      !!case(nf90_short)
        !!select case(variable%ndims)
        !!case(1)
          !!allocate(variable%value%double_1(variable%len(1)))
          !!variable%value%double_1 = real(variable%value%short_1, 8) * scale_factor 
          !!deallocate(variable%value%short_1)
        !!case(2)
          !!allocate(variable%value%double_2(variable%len(1), variable%len(2)))
          !!variable%value%double_2 = real(variable%value%short_2, 8) * scale_factor
        !!case(3)
          !!allocate(variable%value%double_3(variable%len(1), variable%len(2), variable%len(2)))
          !!variable%value%double_3 = real(variable%value%short_3, 8) * scale_factor
        !!end select
      !!case(nf90_ushort)
      !!case(nf90_int)
        !!select case(variable%ndims)
        !!case(1)
          !!allocate(variable%value%double_1(variable%len(1)))
          !!variable%value%double_1 = real(variable%value%int_1, 8) * scale_factor 
        !!case(2)
          !!allocate(variable%value%double_2(variable%len(1), variable%len(2)))
          !!variable%value%double_2 = real(variable%value%int_2, 8) * scale_factor
        !!case(3)
          !!allocate(variable%value%double_3(variable%len(1), variable%len(2), variable%len(2)))
          !!variable%value%double_3 = real(variable%value%int_3, 8) * scale_factor
        !!end select
      !!case(nf90_uint)
      !!case(nf90_int64)
        !!select case(variable%ndims)
        !!case(1)
          !!allocate(variable%value%double_1(variable%len(1)))
          !!variable%value%double_1 = real(variable%value%int64_1, 8) * scale_factor 
        !!case(2)
          !!allocate(variable%value%double_2(variable%len(1), variable%len(2)))
          !!variable%value%double_2 = real(variable%value%int64_2, 8) * scale_factor
        !!case(3)
          !!allocate(variable%value%double_3(variable%len(1), variable%len(2), variable%len(2)))
          !!variable%value%double_3 = real(variable%value%int64_3, 8) * scale_factor
        !!end select
      !!case(nf90_uint64)
      !!case(nf90_float)
        !!select case(variable%ndims)
        !!case(1)
          !!allocate(variable%value%double_1(variable%len(1)))
          !!variable%value%double_1 = real(variable%value%float_1, 8) * scale_factor 
        !!case(2)
          !!allocate(variable%value%double_2(variable%len(1), variable%len(2)))
          !!variable%value%double_2 = real(variable%value%float_2, 8) * scale_factor
        !!case(3)
          !!allocate(variable%value%double_3(variable%len(1), variable%len(2), variable%len(2)))
          !!variable%value%double_3 = real(variable%value%float_3, 8) * scale_factor
        !!end select
      !!case(nf90_double)
      !!case(nf90_char)
      !!case(nf90_string)
      !!end select
    !!end if
  !!end do

