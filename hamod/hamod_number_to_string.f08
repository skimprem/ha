function number_to_string_real_8(value, frmt)
  implicit none
  real(8), intent(in) :: value
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string
  character(len=:), allocatable :: number_to_string_real_8
  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if
  number_to_string_real_8 = trim(adjustl(string))
  return
end function number_to_string_real_8
function number_to_string_real_4(value, frmt)
  implicit none
  real(4), intent(in) :: value
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string
  character(len=:), allocatable :: number_to_string_real_4
  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if
  number_to_string_real_4 = trim(adjustl(string))
  return
end function number_to_string_real_4
function number_to_string_int_4(value, frmt)
  implicit none
  integer(4), intent(in) :: value
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string
  character(len=:), allocatable :: number_to_string_int_4
  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if
  number_to_string_int_4 = trim(adjustl(string))
  return
end function number_to_string_int_4
function number_to_string_int_2(value, frmt)
  implicit none
  integer(2), intent(in) :: value
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string
  character(len=:), allocatable :: number_to_string_int_2
  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if
  number_to_string_int_2 = trim(adjustl(string))
  return
end function number_to_string_int_2
function number_to_string_int_1(value, frmt)
  implicit none
  integer(1), intent(in) :: value
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string
  character(len=:), allocatable :: number_to_string_int_1
  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if
  number_to_string_int_1 = trim(adjustl(string))
  return
end function number_to_string_int_1
function number_to_string_int_8(value, frmt)
  implicit none
  integer(8), intent(in) :: value
  character(*), intent(in), optional :: frmt
  character(max_string_value) :: string
  character(len=:), allocatable :: number_to_string_int_8
  if(present(frmt)) then
    write(string, frmt) value
  else
    write(string, *) value
  end if
  number_to_string_int_8 = trim(adjustl(string))
  return
end function number_to_string_int_8
