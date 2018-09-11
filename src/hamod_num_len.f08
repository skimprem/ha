integer function num_len_real_4(value, frmt)

  implicit none
  real(4), intent(in) :: value 
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string

  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if

  num_len_real_4 = len_trim(adjustl(string))

  return

end function num_len_real_4

integer function num_len_real_8(value, frmt)

  implicit none
  real(8), intent(in) :: value 
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string

  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if

  num_len_real_8 = len_trim(adjustl(string))

  return

end function num_len_real_8

integer function num_len_int_1(value, frmt)

  implicit none
  integer(1), intent(in) :: value
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string

  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if

  num_len_int_1 = len_trim(adjustl(string))

  return

end function num_len_int_1

integer function num_len_int_2(value, frmt)

  implicit none
  integer(2), intent(in) :: value
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string

  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if

  num_len_int_2 = len_trim(adjustl(string))

  return

end function num_len_int_2

integer function num_len_int_4(value, frmt)

  implicit none
  integer(4), intent(in) :: value
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string

  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if

  num_len_int_4 = len_trim(adjustl(string))

  return

end function num_len_int_4

integer function num_len_int_8(value, frmt)

  implicit none
  integer(8), intent(in) :: value
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string

  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if

  num_len_int_8 = len_trim(adjustl(string))

  return

end function num_len_int_8
