  integer, parameter :: max_name_value = 1000
  integer, parameter :: max_string_value = 1000

  type haoptions
    logical :: definition
    character(:), allocatable :: option_name
    character(:), allocatable :: value
  end type

  interface num_len
    module procedure num_len_real_4, num_len_real_8, num_len_int_2, num_len_int_4, num_len_int_1, num_len_int_8
  end interface num_len
 
  interface number_to_string
    module procedure number_to_string_int_1, number_to_string_int_2, number_to_string_int_4, number_to_string_int_8,&
                     number_to_string_real_4, number_to_string_real_8
  end interface number_to_string
