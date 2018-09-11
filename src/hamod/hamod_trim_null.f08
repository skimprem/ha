function trim_null(string)
  implicit none
  character(*), intent(in) :: string
  character(:), allocatable :: trim_null
  integer(4) :: i, n_char, i_char
  trim_null = string
  n_char = len(string)
  do i = 0, n_char - 1
    i_char = n_char - i
    if(string(i_char:i_char) == '0' .and. string(i_char-1:i_char-1) /= '.') then
      trim_null(i_char:i_char) = ' '
    else if(string(i_char:i_char) == ' ') then
      cycle
    else
      trim_null = trim(trim_null)
      return
    end if
  end do
  trim_null = trim(trim_null)
  return
end function
