type ncdimensions
  character(len=:), allocatable :: name
  integer(kind=4) :: len
  !integer(kind=1), allocatable :: value_byte_1(:), value_byte_2(:,:), value_byte_3(:,:,:)
  !integer(kind=2), allocatable :: value_short_1(:), value_short_2(:,:), value_short_3(:,:,:)
  !integer(kind=4), allocatable :: value_int_1(:), value_int_2(:,:), value_int_3(:,:,:)
  !integer(kind=8), allocatable :: value_int64_1(:), value_int64_2(:,:), value_int64_3(:,:,:)
  !real(kind=4), allocatable :: value_float_1(:), value_float_2(:,:), value_float_3(:,:,:)
  !real(kind=8), allocatable :: value_double_1(:), value_double_2(:,:), value_double_3(:,:,:)
  !character(kind=1, len=1), allocatable :: value_char_1(:), value_char_2(:,:), value_char_3(:,:,:)
  !character(kind=1, len=:), allocatable :: value_string_1(:), value_string_2(:,:), value_string_3(:,:,:)
  !integer(kind=8) :: mem_bits
end type ncdimensions
type ncattributes
  character(len=:), allocatable :: name
  integer(kind=4) :: xtype, len 
  integer(kind=1), allocatable :: value_byte(:)
  integer(kind=2), allocatable :: value_short(:)
  integer(kind=4), allocatable :: value_int(:)
  integer(kind=8), allocatable :: value_int64(:)
  real(kind=4), allocatable :: value_float(:)
  real(kind=8), allocatable :: value_double(:)
  character(kind=1, len=1), allocatable :: value_char(:)
  character(kind=1, len=:), allocatable :: value_string(:)
end type ncattributes
type ncvariables
  character(len=:), allocatable :: name
  integer(kind=4) ::  xtype, ndims, natts
  integer(kind=4), dimension(:), allocatable :: dimids
  type(ncdimensions), dimension(:), allocatable :: dimension
  type(ncattributes), dimension(:), allocatable :: attribute
end type ncvariables
type ncfile
  character(len=:), allocatable :: path
  integer(kind=4) :: cmode, mode, ncid, initialsize, chunksize, fillmode, old_mode,&
  h_minfree, v_align, v_minfree, r_align, unlimiteddimid, formatnum, ndimensions, nvariables, nattributes
  type(ncvariables), dimension(:), allocatable :: variable
  type(ncattributes), dimension(:), allocatable :: attribute
end type ncfile
!interface nc_variable_conv
  !module procedure nc_variable_conv_byte_one,&
                   !nc_variable_conv_short_one,&
                   !nc_variable_conv_int_one,&
                   !nc_variable_conv_int64_one,&
                   !nc_variable_conv_float_one,&
                   !nc_variable_conv_double_one,&
                   !nc_variable_conv_byte_two,&
                   !nc_variable_conv_short_two,&
                   !nc_variable_conv_int_two,&
                   !nc_variable_conv_int64_two,&
                   !nc_variable_conv_float_two,&
                   !nc_variable_conv_double_two,&
                   !nc_variable_conv_byte_three,&
                   !nc_variable_conv_short_three,&
                   !nc_variable_conv_int_three,&
                   !nc_variable_conv_int64_three,&
                   !nc_variable_conv_float_three,&
                   !nc_variable_conv_double_three
!end interface nc_variable_conv
!interface nc_value_conv
  !module procedure nc_value_conv_byte_one,&
                   !nc_value_conv_short_one,&
                   !nc_value_conv_int_one,&
                   !nc_value_conv_int64_one,&
                   !nc_value_conv_float_one,&
                   !nc_value_conv_double_one,&
                   !nc_value_conv_byte_two,&
                   !nc_value_conv_short_two,&
                   !nc_value_conv_int_two,&
                   !nc_value_conv_int64_two,&
                   !nc_value_conv_float_two,&
                   !nc_value_conv_double_two,&
                   !nc_value_conv_byte_three,&
                   !nc_value_conv_short_three,&
                   !nc_value_conv_int_three,&
                   !nc_value_conv_int64_three,&
                   !nc_value_conv_float_three,&
                   !nc_value_conv_double_three
!end interface nc_value_conv
