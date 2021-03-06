!###################################################################
! ONE DIMENSIONS
!###################################################################
function value_conv_byte_one_dim(input_value)
  implicit none
  integer(kind=1), intent(in), dimension(:) :: input_value
  real(8), dimension(:), allocatable :: value_conv_byte_one_dim
  allocate(value_conv_byte_one_dim(size(input_value)))
  value_conv_byte_one_dim = real(input_value, 8)
end function value_conv_byte_one_dim
function value_conv_short_one_dim(input_value)
  implicit none
  integer(kind=2), intent(in), dimension(:) :: input_value
  real(8), dimension(:), allocatable :: value_conv_short_one_dim
  allocate(value_conv_short_one_dim(size(input_value)))
  value_conv_short_one_dim = real(input_value, 8)
end function value_conv_short_one_dim
function value_conv_int_one_dim(input_value)
  implicit none
  integer(kind=4), intent(in), dimension(:) :: input_value
  real(8), dimension(:), allocatable :: value_conv_int_one_dim
  allocate(value_conv_int_one_dim(size(input_value)))
  value_conv_int_one_dim = real(input_value, 8)
end function value_conv_int_one_dim
function value_conv_int64_one_dim(input_value)
  implicit none
  integer(kind=8), intent(in), dimension(:) :: input_value
  real(8), dimension(:), allocatable :: value_conv_int64_one_dim
  allocate(value_conv_int64_one_dim(size(input_value)))
  value_conv_int64_one_dim = real(input_value, 8)
end function value_conv_int64_one_dim
function value_conv_float_one_dim(input_value)
  implicit none
  real(kind=4), intent(in), dimension(:) :: input_value
  real(8), dimension(:), allocatable :: value_conv_float_one_dim
  allocate(value_conv_float_one_dim(size(input_value)))
  value_conv_float_one_dim = real(input_value, 8)
end function value_conv_float_one_dim
function value_conv_double_one_dim(input_value)
  implicit none
  real(kind=8), intent(in), dimension(:) :: input_value
  real(8), dimension(:), allocatable :: value_conv_double_one_dim
  allocate(value_conv_double_one_dim(size(input_value)))
  value_conv_double_one_dim = real(input_value, 8)
end function value_conv_double_one_dim
!###################################################################
! TWO DIMENSIONS
!###################################################################
function value_conv_byte_two_dim(input_value)
  implicit none
  integer(kind=1), intent(in), dimension(:,:) :: input_value
  real(8), dimension(:,:), allocatable :: value_conv_byte_two_dim
  allocate(value_conv_byte_two_dim(size(input_value, 1), size(input_value, 2)))
  value_conv_byte_two_dim = real(input_value, 8)
end function value_conv_byte_two_dim
function value_conv_short_two_dim(input_value)
  implicit none
  integer(kind=2), intent(in), dimension(:,:) :: input_value
  real(8), dimension(:,:), allocatable :: value_conv_short_two_dim
  allocate(value_conv_short_two_dim(size(input_value, 1), size(input_value, 2)))
  value_conv_short_two_dim = real(input_value, 8)
end function value_conv_short_two_dim
function value_conv_int_two_dim(input_value)
  implicit none
  integer(kind=4), intent(in), dimension(:,:) :: input_value
  real(8), dimension(:,:), allocatable :: value_conv_int_two_dim
  allocate(value_conv_int_two_dim(size(input_value, 1), size(input_value, 2)))
  value_conv_int_two_dim = real(input_value, 8)
end function value_conv_int_two_dim
function value_conv_int64_two_dim(input_value)
  implicit none
  integer(kind=8), intent(in), dimension(:,:) :: input_value
  real(8), dimension(:,:), allocatable :: value_conv_int64_two_dim
  allocate(value_conv_int64_two_dim(size(input_value, 1), size(input_value, 2)))
  value_conv_int64_two_dim = real(input_value, 8)
end function value_conv_int64_two_dim
function value_conv_float_two_dim(input_value)
  implicit none
  real(kind=4), intent(in), dimension(:,:) :: input_value
  real(8), dimension(:,:), allocatable :: value_conv_float_two_dim
  allocate(value_conv_float_two_dim(size(input_value, 1), size(input_value, 2)))
  value_conv_float_two_dim = real(input_value, 8)
end function value_conv_float_two_dim
function value_conv_double_two_dim(input_value)
  implicit none
  real(kind=8), intent(in), dimension(:,:) :: input_value
  real(8), dimension(:,:), allocatable :: value_conv_double_two_dim
  allocate(value_conv_double_two_dim(size(input_value, 1), size(input_value, 2)))
  value_conv_double_two_dim = real(input_value, 8)
end function value_conv_double_two_dim
!###################################################################
! THREE DIMENSIONS
!###################################################################
function value_conv_byte_three_dim(input_value)
  implicit none
  integer(kind=1), intent(in), dimension(:,:,:) :: input_value
  real(8), dimension(:,:,:), allocatable :: value_conv_byte_three_dim
  allocate(value_conv_byte_three_dim(size(input_value, 1), size(input_value, 2),&
    size(input_value, 3)))
  value_conv_byte_three_dim = real(input_value, 8)
end function value_conv_byte_three_dim
function value_conv_short_three_dim(input_value)
  implicit none
  integer(kind=2), intent(in), dimension(:,:,:) :: input_value
  real(8), dimension(:,:,:), allocatable :: value_conv_short_three_dim
  allocate(value_conv_short_three_dim(size(input_value, 1), size(input_value, 2),&
    size(input_value)))
  value_conv_short_three_dim = real(input_value, 8)
end function value_conv_short_three_dim
function value_conv_int_three_dim(input_value)
  implicit none
  integer(kind=4), intent(in), dimension(:,:,:) :: input_value
  real(8), dimension(:,:,:), allocatable :: value_conv_int_three_dim
  allocate(value_conv_int_three_dim(size(input_value, 1), size(input_value, 2),&
    size(input_value, 3)))
  value_conv_int_three_dim = real(input_value, 8)
end function value_conv_int_three_dim
function value_conv_int64_three_dim(input_value)
  implicit none
  integer(kind=8), intent(in), dimension(:,:,:) :: input_value
  real(8), dimension(:,:,:), allocatable :: value_conv_int64_three_dim
  allocate(value_conv_int64_three_dim(size(input_value, 1), size(input_value, 2),&
    size(input_value, 3)))
  value_conv_int64_three_dim = real(input_value, 8)
end function value_conv_int64_three_dim
function value_conv_float_three_dim(input_value)
  implicit none
  real(kind=4), intent(in), dimension(:,:,:) :: input_value
  real(8), dimension(:,:,:), allocatable :: value_conv_float_three_dim
  allocate(value_conv_float_three_dim(size(input_value, 1), size(input_value, 2),&
    size(input_value, 3)))
  value_conv_float_three_dim = real(input_value, 8)
end function value_conv_float_three_dim
function value_conv_double_three_dim(input_value)
  implicit none
  real(kind=8), intent(in), dimension(:,:,:) :: input_value
  real(8), dimension(:,:,:), allocatable :: value_conv_double_three_dim
  allocate(value_conv_double_three_dim(size(input_value, 1), size(input_value, 2),&
    size(input_value,3)))
  value_conv_double_three_dim = real(input_value, 8)
end function value_conv_double_three_dim
