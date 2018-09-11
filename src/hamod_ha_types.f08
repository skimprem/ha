integer, parameter :: max_name_value = 1000
integer, parameter :: max_string_value = 1000

type haoptions
  logical :: definition
  character(:), allocatable :: option_name
  character(:), allocatable :: value
end type

interface num_len
  module procedure num_len_real_4,&
                   num_len_real_8,&
                   num_len_int_2,&
                   num_len_int_4,&
                   num_len_int_1,&
                   num_len_int_8
end interface num_len

interface number_to_string
  module procedure number_to_string_int_1,&
                   number_to_string_int_2,&
                   number_to_string_int_4,&
                   number_to_string_int_8,&
                   number_to_string_real_4,&
                   number_to_string_real_8
end interface number_to_string

interface value_conv
  module procedure value_conv_byte_one_dim,&
                   value_conv_short_one_dim,&
                   value_conv_int_one_dim,&
                   value_conv_int64_one_dim,&
                   value_conv_float_one_dim,&
                   value_conv_double_one_dim,&
                   value_conv_byte_two_dim,&
                   value_conv_short_two_dim,&
                   value_conv_int_two_dim,&
                   value_conv_int64_two_dim,&
                   value_conv_float_two_dim,&
                   value_conv_double_two_dim,&
                   value_conv_byte_three_dim,&
                   value_conv_short_three_dim,&
                   value_conv_int_three_dim,&
                   value_conv_int64_three_dim,&
                   value_conv_float_three_dim,&
                   value_conv_double_three_dim
end interface value_conv
