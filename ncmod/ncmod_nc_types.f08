type ncdimensions
  character(len=:), allocatable :: name
  integer(kind=4) :: dimid, len 
  integer(kind=1), allocatable :: byte_1(:), byte_2(:,:), byte_3(:,:,:)
  integer(kind=2), allocatable :: short_1(:), short_2(:,:), short_3(:,:,:)
  integer(kind=4), allocatable :: int_1(:), int_2(:,:), int_3(:,:,:)
  integer(kind=8), allocatable :: int64_1(:), int64_2(:,:), int64_3(:,:,:)
  real(kind=4), allocatable :: float_1(:), float_2(:,:), float_3(:,:,:)
  real(kind=8), allocatable :: double_1(:), double_2(:,:), double_3(:,:,:)
  character(kind=1, len=1), allocatable ::char_1(:), char_2(:,:), char_3(:,:,:)
  character(kind=1, len=:), allocatable :: string_1(:), string_2(:,:), string_3(:,:,:)
  integer(kind=8) :: mem_bits
end type ncdimensions
type ncattributes
  character(len=:), allocatable :: name
  integer(kind=4) :: xtype, len, attnum 
  integer(kind=1), allocatable :: byte(:)
  integer(kind=2), allocatable :: short(:)
  integer(kind=4), allocatable :: int(:)
  integer(kind=8), allocatable :: int64(:)
  real(kind=4), allocatable :: float(:)
  real(kind=8), allocatable :: double(:)
  character(kind=1, len=1), allocatable :: char(:)
  character(kind=1, len=:), allocatable :: string(:)
end type ncattributes
type ncvariables
  character(len=:), allocatable :: name
  integer(kind=4) ::  varid, xtype, ndims, natts
  integer(kind=4), dimension(:), allocatable :: dimids
  integer(kind=4), dimension(:), allocatable :: len
  type(ncattributes), dimension(:), allocatable :: attribute
end type ncvariables
type ncfile
  character(len=:), allocatable :: path
  !character(len=:), allocatable :: name, title, history, conventions
  integer(kind=4) :: cmode, ncid, ndimensions, nvariables, nattributes, unlimiteddimid, formatnum,&
                     chunksize
  type(ncdimensions), dimension(:), allocatable :: dimension
  type(ncvariables), dimension(:), allocatable :: variable
  type(ncattributes), dimension(:), allocatable :: attribute
end type ncfile
interface nc_variable_conv
  module procedure nc_variable_conv_byte_one,&
                   nc_variable_conv_short_one,&
                   nc_variable_conv_int_one,&
                   nc_variable_conv_int64_one,&
                   nc_variable_conv_float_one,&
                   nc_variable_conv_double_one,&
                   nc_variable_conv_byte_two,&
                   nc_variable_conv_short_two,&
                   nc_variable_conv_int_two,&
                   nc_variable_conv_int64_two,&
                   nc_variable_conv_float_two,&
                   nc_variable_conv_double_two,&
                   nc_variable_conv_byte_three,&
                   nc_variable_conv_short_three,&
                   nc_variable_conv_int_three,&
                   nc_variable_conv_int64_three,&
                   nc_variable_conv_float_three,&
                   nc_variable_conv_double_three
end interface nc_variable_conv
interface nc_value_conv
  module procedure nc_value_conv_byte_one,&
                   nc_value_conv_short_one,&
                   nc_value_conv_int_one,&
                   nc_value_conv_int64_one,&
                   nc_value_conv_float_one,&
                   nc_value_conv_double_one,&
                   nc_value_conv_byte_two,&
                   nc_value_conv_short_two,&
                   nc_value_conv_int_two,&
                   nc_value_conv_int64_two,&
                   nc_value_conv_float_two,&
                   nc_value_conv_double_two,&
                   nc_value_conv_byte_three,&
                   nc_value_conv_short_three,&
                   nc_value_conv_int_three,&
                   nc_value_conv_int64_three,&
                   nc_value_conv_float_three,&
                   nc_value_conv_double_three
end interface nc_value_conv
