!#########################################################
! ONE DIM
!#########################################################
subroutine nc_value_conv_byte_one_dim(value, output_value)
  implicit none
  type(ncvalues) :: value
  integer(kind=1), dimension(:), allocatable :: output_value
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
  type(ncvalues), intent(in) :: value
  integer(kind=2), dimension(:), allocatable :: output_value
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
  type(ncvalues), intent(in) :: value
  integer(kind=4), dimension(:), allocatable :: output_value
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
  type(ncvalues), intent(in) :: value
  integer(kind=8), dimension(:), allocatable :: output_value
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
  type(ncvalues), intent(in) :: value
  real(kind=4), dimension(:), allocatable :: output_value
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
  type(ncvalues), intent(in) :: value
  real(kind=8), dimension(:), allocatable :: output_value
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
  type(ncvalues), intent(in) :: value
  integer(kind=1), dimension(:,:), allocatable :: output_value
  if(allocated(value%byte_2)) then
    allocate(output_value(size(value%byte_2, 1), size(value%byte_2, 2)))
    output_value = value%byte_2
  else if(allocated(value%short_2)) then
    allocate(output_value(size(value%short_2, 1), size(value%short_2, 2)))
    output_value = int(value%short_2, 1)
  else if(allocated(value%int_2)) then
    allocate(output_value(size(value%int_2, 1), size(value%int_2, 2)))
    output_value = int(value%int_2, 1)
  else if(allocated(value%int64_2)) then
    allocate(output_value(size(value%int64_2, 1), size(value%int64_2, 2)))
    output_value = int(value%int64_2, 1)
  else if(allocated(value%float_2)) then
    allocate(output_value(size(value%float_2, 1), size(value%float_2, 2)))
    output_value = int(value%float_2, 1)
  else if(allocated(value%double_2)) then
    allocate(output_value(size(value%double_2, 1), size(value%double_2, 2)))
    output_value = int(value%double_2, 1)
  end if
end subroutine nc_value_conv_byte_two_dim
subroutine nc_value_conv_short_two_dim(value, output_value)
  implicit none
  type(ncvalues), intent(in) :: value
  integer(kind=2), dimension(:,:), allocatable :: output_value
  if(allocated(value%byte_2)) then
    allocate(output_value(size(value%byte_2, 1), size(value%byte_2, 2)))
    output_value = int(value%byte_2, 2)
  else if(allocated(value%short_2)) then
    allocate(output_value(size(value%short_2, 1), size(value%short_2, 2)))
    output_value = value%short_2
  else if(allocated(value%int_2)) then
    allocate(output_value(size(value%int_2, 1), size(value%int_2, 2)))
    output_value = int(value%int_2, 2)
  else if(allocated(value%int64_2)) then
    allocate(output_value(size(value%int64_2, 1), size(value%int64_2, 2)))
    output_value = int(value%int64_2, 2)
  else if(allocated(value%float_2)) then
    allocate(output_value(size(value%float_2, 1), size(value%float_2, 2)))
    output_value = int(value%float_2, 2)
  else if(allocated(value%double_2)) then
    allocate(output_value(size(value%double_2, 1), size(value%double_2, 2)))
    output_value = int(value%double_2, 2)
  end if
end subroutine nc_value_conv_short_two_dim
subroutine nc_value_conv_int_two_dim(value, output_value)
  implicit none
  type(ncvalues), intent(in) :: value
  integer(kind=4), dimension(:,:), allocatable :: output_value
  if(allocated(value%byte_2)) then
    allocate(output_value(size(value%byte_2, 1), size(value%byte_2, 2)))
    output_value = int(value%byte_2, 4)
  else if(allocated(value%short_2)) then
    allocate(output_value(size(value%short_2, 1), size(value%short_2, 2)))
    output_value = int(value%short_2, 4)
  else if(allocated(value%int_2)) then
    allocate(output_value(size(value%int_2, 1), size(value%int_2, 2)))
    output_value = value%int_2
  else if(allocated(value%int64_2)) then
    allocate(output_value(size(value%int64_2, 1), size(value%int64_2, 2)))
    output_value = int(value%int64_2, 4)
  else if(allocated(value%float_2)) then
    allocate(output_value(size(value%float_2, 1), size(value%float_2, 2)))
    output_value = int(value%float_2, 4)
  else if(allocated(value%double_2)) then
    allocate(output_value(size(value%double_2, 1), size(value%double_2, 2)))
    output_value = int(value%double_2, 4)
  end if
end subroutine nc_value_conv_int_two_dim
subroutine nc_value_conv_int64_two_dim(value, output_value)
  implicit none
  type(ncvalues), intent(in) :: value
  integer(kind=8), dimension(:,:), allocatable :: output_value
  if(allocated(value%byte_2)) then
    allocate(output_value(size(value%byte_2, 1), size(value%byte_2, 2)))
    output_value = int(value%byte_2, 8)
  else if(allocated(value%short_2)) then
    allocate(output_value(size(value%short_2, 1), size(value%short_2, 2)))
    output_value = int(value%short_2, 8)
  else if(allocated(value%int_2)) then
    allocate(output_value(size(value%int_2, 1), size(value%int_2, 2)))
    output_value = int(value%int_2, 8)
  else if(allocated(value%int64_2)) then
    allocate(output_value(size(value%int64_2, 1), size(value%int64_2, 2)))
    output_value = value%int64_2
  else if(allocated(value%float_2)) then
    allocate(output_value(size(value%float_2, 1), size(value%float_2, 2)))
    output_value = int(value%float_2, 8)
  else if(allocated(value%double_2)) then
    allocate(output_value(size(value%double_2, 1), size(value%double_2, 2)))
    output_value = int(value%double_2, 8)
  end if
end subroutine nc_value_conv_int64_two_dim
subroutine nc_value_conv_float_two_dim(value, output_value)
  implicit none
  type(ncvalues), intent(in) :: value
  real(kind=4), dimension(:,:), allocatable :: output_value
  if(allocated(value%byte_2)) then
    allocate(output_value(size(value%byte_2, 1), size(value%byte_2, 2)))
    output_value = real(value%byte_2, 4)
  else if(allocated(value%short_2)) then
    allocate(output_value(size(value%short_2, 1), size(value%short_2, 2)))
    output_value = real(value%short_2, 4)
  else if(allocated(value%int_2)) then
    allocate(output_value(size(value%int_2, 1), size(value%int_2, 2)))
    output_value = real(value%int_2, 4)
  else if(allocated(value%int64_2)) then
    allocate(output_value(size(value%int64_2, 1), size(value%int64_2, 2)))
    output_value = real(value%int64_2, 4)
  else if(allocated(value%float_2)) then
    allocate(output_value(size(value%float_2, 1), size(value%float_2, 2)))
    output_value = value%float_2
  else if(allocated(value%double_2)) then
    allocate(output_value(size(value%double_2, 1), size(value%double_2, 2)))
    output_value = real(value%double_2, 4)
  end if
end subroutine nc_value_conv_float_two_dim
subroutine nc_value_conv_double_two_dim(value, output_value)
  implicit none
  type(ncvalues), intent(in) :: value
  real(kind=8), dimension(:,:), allocatable :: output_value
  if(allocated(value%byte_2)) then
    allocate(output_value(size(value%byte_2, 1), size(value%byte_2, 2)))
    output_value = real(value%byte_2, 8)
  else if(allocated(value%short_2)) then
    allocate(output_value(size(value%short_2, 1), size(value%short_2, 2)))
    output_value = real(value%short_2, 8)
  else if(allocated(value%int_2)) then
    allocate(output_value(size(value%int_2, 1), size(value%int_2, 2)))
    output_value = real(value%int_2, 8)
  else if(allocated(value%int64_2)) then
    allocate(output_value(size(value%int64_2, 1), size(value%int64_2, 2)))
    output_value = real(value%int64_2, 8)
  else if(allocated(value%float_2)) then
    allocate(output_value(size(value%float_2, 1), size(value%float_2, 2)))
    output_value = real(value%float_2, 8)
  else if(allocated(value%double_2)) then
    allocate(output_value(size(value%double_2, 1), size(value%double_2, 2)))
    output_value = value%double_2
  end if
end subroutine nc_value_conv_double_two_dim
!#########################################################
! THREE DIM
!#########################################################
subroutine nc_value_conv_byte_three_dim(value, output_value)
  implicit none
  type(ncvalues), intent(in) :: value
  integer(kind=1), dimension(:,:,:), allocatable :: output_value
  if(allocated(value%byte_3)) then
    allocate(output_value(size(value%byte_3, 1), size(value%byte_3, 2), size(value%byte_3, 3)))
    output_value = value%byte_3
  else if(allocated(value%short_3)) then
    allocate(output_value(size(value%short_3, 1), size(value%short_3, 2), size(value%short_3, 3)))
    output_value = int(value%short_3, 1)
  else if(allocated(value%int_3)) then
    allocate(output_value(size(value%int_3, 1), size(value%int_3, 2), size(value%int_3, 3)))
    output_value = int(value%int_3, 1)
  else if(allocated(value%int64_3)) then
    allocate(output_value(size(value%int64_3, 1), size(value%int64_3, 2), size(value%int64_3, 3)))
    output_value = int(value%int64_3, 1)
  else if(allocated(value%float_3)) then
    allocate(output_value(size(value%float_3, 1), size(value%float_3, 2), size(value%float_3, 3)))
    output_value = int(value%float_3, 1)
  else if(allocated(value%double_3)) then
    allocate(output_value(size(value%double_3, 1), size(value%double_3, 2), size(value%double_3, 3)))
    output_value = int(value%double_3, 1)
  end if
end subroutine nc_value_conv_byte_three_dim
subroutine nc_value_conv_short_three_dim(value, output_value)
  implicit none
  type(ncvalues), intent(in) :: value
  integer(kind=2), dimension(:,:,:), allocatable :: output_value
  if(allocated(value%byte_3)) then
    allocate(output_value(size(value%byte_3, 1), size(value%byte_3, 2), size(value%byte_3, 3)))
    output_value = int(value%byte_3, 2)
  else if(allocated(value%short_3)) then
    allocate(output_value(size(value%short_3, 1), size(value%short_3, 2), size(value%short_3, 3)))
    output_value = value%short_3
  else if(allocated(value%int_3)) then
    allocate(output_value(size(value%int_3, 1), size(value%int_3, 2), size(value%int_3, 3)))
    output_value = int(value%int_3, 2)
  else if(allocated(value%int64_3)) then
    allocate(output_value(size(value%int64_3, 1), size(value%int64_3, 2), size(value%int64_3, 3)))
    output_value = int(value%int64_3, 2)
  else if(allocated(value%float_3)) then
    allocate(output_value(size(value%float_3, 1), size(value%float_3, 2), size(value%float_3, 3)))
    output_value = int(value%float_3, 2)
  else if(allocated(value%double_3)) then
    allocate(output_value(size(value%double_3, 1), size(value%double_3, 2), size(value%double_3, 3)))
    output_value = int(value%double_3, 2)
  end if
end subroutine nc_value_conv_short_three_dim
subroutine nc_value_conv_int_three_dim(value, output_value)
  implicit none
  type(ncvalues), intent(in) :: value
  integer(kind=4), dimension(:,:,:), allocatable :: output_value
  if(allocated(value%byte_3)) then
    allocate(output_value(size(value%byte_3, 1), size(value%byte_3, 2), size(value%byte_3, 3)))
    output_value = int(value%byte_3, 4)
  else if(allocated(value%short_3)) then
    allocate(output_value(size(value%short_3, 1), size(value%short_3, 2), size(value%short_3, 3)))
    output_value = int(value%short_3, 4)
  else if(allocated(value%int_3)) then
    allocate(output_value(size(value%int_3, 1), size(value%int_3, 2), size(value%int_3, 3)))
    output_value = value%int_3
  else if(allocated(value%int64_3)) then
    allocate(output_value(size(value%int64_3, 1), size(value%int64_3, 2), size(value%int64_3, 3)))
    output_value = int(value%int64_3, 4)
  else if(allocated(value%float_3)) then
    allocate(output_value(size(value%float_3, 1), size(value%float_3, 2), size(value%float_3, 3)))
    output_value = int(value%float_3, 4)
  else if(allocated(value%double_3)) then
    allocate(output_value(size(value%double_3, 1), size(value%double_3, 2), size(value%double_3, 3)))
    output_value = int(value%double_3, 4)
  end if
end subroutine nc_value_conv_int_three_dim
subroutine nc_value_conv_int64_three_dim(value, output_value)
  implicit none
  type(ncvalues), intent(in) :: value
  integer(kind=8), dimension(:,:,:), allocatable :: output_value
  if(allocated(value%byte_3)) then
    allocate(output_value(size(value%byte_3, 1), size(value%byte_3, 2), size(value%byte_3, 3)))
    output_value = int(value%byte_3, 8)
  else if(allocated(value%short_3)) then
    allocate(output_value(size(value%short_3, 1), size(value%short_3, 2), size(value%short_3, 3)))
    output_value = int(value%short_3, 8)
  else if(allocated(value%int_3)) then
    allocate(output_value(size(value%int_3, 1), size(value%int_3, 2), size(value%int_3, 3)))
    output_value = int(value%int_3, 8)
  else if(allocated(value%int64_3)) then
    allocate(output_value(size(value%int64_3, 1), size(value%int64_3, 2), size(value%int64_3, 3)))
    output_value = value%int64_3
  else if(allocated(value%float_3)) then
    allocate(output_value(size(value%float_3, 1), size(value%float_3, 2), size(value%float_3, 3)))
    output_value = int(value%float_3, 8)
  else if(allocated(value%double_3)) then
    allocate(output_value(size(value%double_3, 1), size(value%double_3, 2), size(value%double_3, 3)))
    output_value = int(value%double_3, 8)
  end if
end subroutine nc_value_conv_int64_three_dim
subroutine nc_value_conv_float_three_dim(value, output_value)
  implicit none
  type(ncvalues), intent(in) :: value
  real(kind=4), dimension(:,:,:), allocatable :: output_value
  if(allocated(value%byte_3)) then
    allocate(output_value(size(value%byte_3, 1), size(value%byte_3, 2), size(value%byte_3, 3)))
    output_value = real(value%byte_3, 4)
  else if(allocated(value%short_3)) then
    allocate(output_value(size(value%short_3, 1), size(value%short_3, 2), size(value%short_3, 3)))
    output_value = real(value%short_3, 4)
  else if(allocated(value%int_3)) then
    allocate(output_value(size(value%int_3, 1), size(value%int_3, 2), size(value%int_3, 3)))
    output_value = real(value%int_3, 4)
  else if(allocated(value%int64_3)) then
    allocate(output_value(size(value%int64_3, 1), size(value%int64_3, 2), size(value%int64_3, 3)))
    output_value = real(value%int64_3, 4)
  else if(allocated(value%float_3)) then
    allocate(output_value(size(value%float_3, 1), size(value%float_3, 2), size(value%float_3, 3)))
    output_value = value%float_3
  else if(allocated(value%double_3)) then
    allocate(output_value(size(value%double_3, 1), size(value%double_3, 2), size(value%double_3, 3)))
    output_value = real(value%double_3, 4)
  end if
end subroutine nc_value_conv_float_three_dim
subroutine nc_value_conv_double_three_dim(value, output_value)
  implicit none
  type(ncvalues), intent(in) :: value
  real(kind=8), dimension(:,:,:), allocatable :: output_value
  if(allocated(value%byte_3)) then
    allocate(output_value(size(value%byte_3, 1), size(value%byte_3, 2), size(value%byte_3, 3)))
    output_value = real(value%byte_3, 8)
  else if(allocated(value%short_3)) then
    allocate(output_value(size(value%short_3, 1), size(value%short_3, 2), size(value%short_3, 3)))
    output_value = real(value%short_3, 8)
  else if(allocated(value%int_3)) then
    allocate(output_value(size(value%int_3, 1), size(value%int_3, 2), size(value%int_3, 3)))
    output_value = real(value%int_3, 8)
  else if(allocated(value%int64_3)) then
    allocate(output_value(size(value%int64_3, 1), size(value%int64_3, 2), size(value%int64_3, 3)))
    output_value = real(value%int64_3, 8)
  else if(allocated(value%float_3)) then
    allocate(output_value(size(value%float_3, 1), size(value%float_3, 2), size(value%float_3, 3)))
    output_value = real(value%float_3, 8)
  else if(allocated(value%double_3)) then
    allocate(output_value(size(value%double_3, 1), size(value%double_3, 2), size(value%double_3, 3)))
    output_value = value%double_3
  end if
end subroutine nc_value_conv_double_three_dim
