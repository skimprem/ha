!#########################################################
! ONE DIM
!#########################################################
subroutine nc_value_conv_byte_one_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=1), dimension(:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1)))
    output_value = value%byte_1
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1)))
    output_value = int(value%short_1, 1)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1)))
    output_value = int(value%int_1, 1)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1)))
    output_value = int(value%int64_1, 1)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1)))
    output_value = int(value%float_1, 1)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1)))
    output_value = int(value%double_1, 1)
  end if
end subroutine nc_value_conv_byte_one_dim
subroutine nc_value_conv_short_one_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=2), dimension(:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1)))
    output_value = int(value%byte_1, 2)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1)))
    output_value = value%short_1
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1)))
    output_value = int(value%int_1, 2)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1)))
    output_value = int(value%int64_1, 2)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1)))
    output_value = int(value%float_1, 2)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1)))
    output_value = int(value%double_1, 2)
  end if
end subroutine nc_value_conv_short_one_dim
subroutine nc_value_conv_int_one_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=4), dimension(:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1)))
    output_value = int(value%byte_1, 4)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1)))
    output_value = int(value%short_1, 4)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1)))
    output_value = value%int_1
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1)))
    output_value = int(value%int64_1, 4)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1)))
    output_value = int(value%float_1, 4)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1)))
    output_value = int(value%double_1, 4)
  end if
end subroutine nc_value_conv_int_one_dim
subroutine nc_value_conv_int64_one_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=8), dimension(:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1)))
    output_value = int(value%byte_1, 8)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1)))
    output_value = int(value%short_1, 8)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1)))
    output_value = int(value%int_1, 8)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1)))
    output_value = value%int64_1
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1)))
    output_value = int(value%float_1, 8)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1)))
    output_value = int(value%double_1, 8)
  end if
end subroutine nc_value_conv_int64_one_dim
subroutine nc_value_conv_float_one_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  real(kind=4), dimension(:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1)))
    output_value = real(value%byte_1, 4)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1)))
    output_value = real(value%short_1, 4)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1)))
    output_value = real(value%int_1, 4)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1)))
    output_value = real(value%int64_1, 4)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1)))
    output_value = value%float_1
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1)))
    output_value = real(value%double_1, 4)
  end if
end subroutine nc_value_conv_float_one_dim
subroutine nc_value_conv_double_one_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  real(kind=8), dimension(:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1)))
    output_value = real(value%byte_1, 8)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1)))
    output_value = real(value%short_1, 8)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1)))
    output_value = real(value%int_1, 8)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1)))
    output_value = real(value%int64_1, 8)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1)))
    output_value = real(value%float_1, 8)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1)))
    output_value = value%double_1
  end if
end subroutine nc_value_conv_double_one_dim
!#########################################################
! TWO DIM
!#########################################################
subroutine nc_value_conv_byte_two_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=1), dimension(:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2)))
    output_value = value%byte_1
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2)))
    output_value = int(value%short_1, 1)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2)))
    output_value = int(value%int_1, 1)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2)))
    output_value = int(value%int64_1, 1)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2)))
    output_value = int(value%float_1, 1)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2)))
    output_value = int(value%double_1, 1)
  end if
end subroutine nc_value_conv_byte_two_dim
subroutine nc_value_conv_short_two_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=2), dimension(:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2)))
    output_value = int(value%byte_1, 2)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2)))
    output_value = value%short_1
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2)))
    output_value = int(value%int_1, 2)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2)))
    output_value = int(value%int64_1, 2)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2)))
    output_value = int(value%float_1, 2)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2)))
    output_value = int(value%double_1, 2)
  end if
end subroutine nc_value_conv_short_two_dim
subroutine nc_value_conv_int_two_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=4), dimension(:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2)))
    output_value = int(value%byte_1, 4)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2)))
    output_value = int(value%short_1, 4)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2)))
    output_value = value%int_1
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2)))
    output_value = int(value%int64_1, 4)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2)))
    output_value = int(value%float_1, 4)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2)))
    output_value = int(value%double_1, 4)
  end if
end subroutine nc_value_conv_int_two_dim
subroutine nc_value_conv_int64_two_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=8), dimension(:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2)))
    output_value = int(value%byte_1, 8)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2)))
    output_value = int(value%short_1, 8)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2)))
    output_value = int(value%int_1, 8)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2)))
    output_value = value%int64_1
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2)))
    output_value = int(value%float_1, 8)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2)))
    output_value = int(value%double_1, 8)
  end if
end subroutine nc_value_conv_int64_two_dim
subroutine nc_value_conv_float_two_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  real(kind=4), dimension(:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2)))
    output_value = real(value%byte_1, 4)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2)))
    output_value = real(value%short_1, 4)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2)))
    output_value = real(value%int_1, 4)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2)))
    output_value = real(value%int64_1, 4)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2)))
    output_value = value%float_1
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2)))
    output_value = real(value%double_1, 4)
  end if
end subroutine nc_value_conv_float_two_dim
subroutine nc_value_conv_double_two_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  real(kind=8), dimension(:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2)))
    output_value = real(value%byte_1, 8)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2)))
    output_value = real(value%short_1, 8)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2)))
    output_value = real(value%int_1, 8)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2)))
    output_value = real(value%int64_1, 8)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2)))
    output_value = real(value%float_1, 8)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2)))
    output_value = value%double_1
  end if
end subroutine nc_value_conv_double_two_dim
!#########################################################
! THREE DIM
!#########################################################
subroutine nc_value_conv_byte_three_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=1), dimension(:,:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2), size(value%byte_1, 3)))
    output_value = value%byte_1
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2), size(value%short_1, 3)))
    output_value = int(value%short_1, 1)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2), size(value%int_1, 3)))
    output_value = int(value%int_1, 1)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2), size(value%int64_1, 3)))
    output_value = int(value%int64_1, 1)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2), size(value%float_1, 3)))
    output_value = int(value%float_1, 1)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2), size(value%double_1, 3)))
    output_value = int(value%double_1, 1)
  end if
end subroutine nc_value_conv_byte_three_dim
subroutine nc_value_conv_short_three_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=2), dimension(:,:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2), size(value%byte_1, 3)))
    output_value = int(value%byte_1, 2)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2), size(value%short_1, 3)))
    output_value = value%short_1
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2), size(value%int_1, 3)))
    output_value = int(value%int_1, 2)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2), size(value%int64_1, 3)))
    output_value = int(value%int64_1, 2)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2), size(value%float_1, 3)))
    output_value = int(value%float_1, 2)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2), size(value%double_1, 3)))
    output_value = int(value%double_1, 2)
  end if
end subroutine nc_value_conv_short_three_dim
subroutine nc_value_conv_int_three_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=4), dimension(:,:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2), size(value%byte_1, 3)))
    output_value = int(value%byte_1, 4)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2), size(value%short_1, 3)))
    output_value = int(value%short_1, 4)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2), size(value%int_1, 3)))
    output_value = value%int_1
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2), size(value%int64_1, 3)))
    output_value = int(value%int64_1, 4)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2), size(value%float_1, 3)))
    output_value = int(value%float_1, 4)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2), size(value%double_1, 3)))
    output_value = int(value%double_1, 4)
  end if
end subroutine nc_value_conv_int_three_dim
subroutine nc_value_conv_int64_three_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  integer(kind=8), dimension(:,:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2), size(value%byte_1, 3)))
    output_value = int(value%byte_1, 8)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2), size(value%short_1, 3)))
    output_value = int(value%short_1, 8)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2), size(value%int_1, 3)))
    output_value = int(value%int_1, 8)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2), size(value%int64_1, 3)))
    output_value = value%int64_1
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2), size(value%float_1, 3)))
    output_value = int(value%float_1, 8)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2), size(value%double_1, 3)))
    output_value = int(value%double_1, 8)
  end if
end subroutine nc_value_conv_int64_three_dim
subroutine nc_value_conv_float_tree_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  real(kind=4), dimension(:,:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2), size(value%byte_1, 3)))
    output_value = real(value%byte_1, 4)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2), size(value%short_1, 3)))
    output_value = real(value%short_1, 4)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2), size(value%int_1, 3)))
    output_value = real(value%int_1, 4)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2), size(value%int64_1, 3)))
    output_value = real(value%int64_1, 4)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2), size(value%float_1, 3)))
    output_value = value%float_1
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2), size(value%double_1, 3)))
    output_value = real(value%double_1, 4)
  end if
end subroutine nc_value_conv_float_three_dim
subroutine nc_value_conv_double_three_dim(value, output_value)
  implicit none
  type(ncvalue), intent(in) :: value
  real(kind=8), dimension(:,:,:) :: output_value
  if(allocated(value%byte_1)) then
    allocate(output_value(size(value%byte_1, 1), size(value%byte_1, 2), size(value%byte_1, 3)))
    output_value = real(value%byte_1, 8)
  else if(allocated(value%short_1)) then
    allocate(output_value(size(value%short_1, 1), size(value%short_1, 2), size(value%short_1, 3)))
    output_value = real(value%short_1, 8)
  else if(allocated(value%int_1)) then
    allocate(output_value(size(value%int_1, 1), size(value%int_1, 2), size(value%int_1, 3)))
    output_value = real(value%int_1, 8)
  else if(allocated(value%int64_1)) then
    allocate(output_value(size(value%int64_1, 1), size(value%int64_1, 2), size(value%int64_1, 3)))
    output_value = real(value%int64_1, 8)
  else if(allocated(value%float_1)) then
    allocate(output_value(size(value%float_1, 1), size(value%float_1, 2), size(value%float_1, 3)))
    output_value = real(value%float_1, 8)
  else if(allocated(value%double_1)) then
    allocate(output_value(size(value%double_1, 1), size(value%double_1, 2), size(value%double_1, 3)))
    output_value = value%double_1
  end if
end subroutine nc_value_conv_double_three_dim
