module hamodule

contains

subroutine input_check(check_type, arg, string)

  implicit none

  character(*), intent(in) :: check_type
  character(*), intent(in) :: arg
  character(*), intent(in), optional :: string
  logical :: file_exist = .false., arg_true = .false.
  integer(4) :: i, k = 1

  ! check types:
  !   'noarg' - parameter existence
  !   'nofile' - file existence
  !   'checkarg' - argument is true

  select case(check_type)
  case('noarg')
    if(trim(adjustl(arg)) == '') then
      print '(a)', 'ERROR: Do not set any option!'
      call print_help('stop')
    end if
  case('noopt')
    if(trim(adjustl(arg)) == '') then
      print '(a)', 'ERROR: Do not define parameter of option "'//trim(adjustl(string))//'"'
      call print_help('stop')
    end if
  case('nofile')
    inquire(file=trim(adjustl(arg)), exist=file_exist)
    if(file_exist .eqv. .false.) then
      print '(a)', 'ERROR: No such file "'//trim(adjustl(arg))//'"'
      call print_help('stop')
    end if
  case('unopt')
    print '(a)', 'ERROR: Option "'//trim(adjustl(arg))//'" unrecognized!'
    call print_help('stop')
  case('charg')
    do i = 1, len_trim(string)
      if(string(i:i) == ',') then
        if( trim(adjustl(arg)) == trim(adjustl(string(k:i-1))) ) arg_true = .true.
        k = i + 1
      end if
    end do
    if(trim(adjustl(string(k:))) == trim(adjustl(arg))) arg_true = .true.
    if(arg_true .eqv. .false.) then
      print '(a)', 'ERROR: The parameter '//trim(adjustl(arg))//' is incorrect!'
      call print_help('stop')
    end if
  end select

  return

end subroutine input_check

subroutine print_help(type_help)
  implicit none
  character(*), intent(in), optional :: type_help

  print '(a)', 'Usage: ha [OPTION]... [FILE]...'
  print '(a)', ''
  print '(a)', 'Calculates the coefficients of spherical harmonics over a regular grid' 
  print '(a)', 'using various algorithms'

  if(present(type_help)) then
    select case(type_help)
    case('stop')
      !stop 'Stopped!'
      stop
    end select
  end if

  return
end subroutine print_help

integer(4) function num_len(iv, rv, frmt)

  implicit none
  integer(4), intent(in), optional :: iv
  real, intent(in), optional :: rv
  character(*), intent(in), optional :: frmt
  character(10000) :: string

  if(present(iv)) then
    if(present(frmt)) then
      write(string, frmt) iv
    else
      write(string, *) iv
    end if
  else if(present(rv)) then
    if(present(frmt)) then
      write(string, frmt) rv
    else
      write(string, *) rv
    end if
  else
    string = ''
  end if

  num_len = len_trim(adjustl(string))

  return

end function num_len

function number_to_string(iv, rv, len, frmt)
  
  implicit none
  integer(4), intent(in), optional :: iv
  real, intent(in), optional :: rv
  integer(4), intent(in) :: len
  character(*), intent(in), optional :: frmt
  character(10000) :: string
  character(len=len) :: number_to_string

  if(present(frmt)) then
    if(present(iv)) then
      write(string, frmt) iv
    else if(present(rv)) then
      write(string, frmt) rv
    else
      string = ''
    end if
  else
    if(present(iv)) then
      write(string, *) iv
    else if(present(rv)) then
      write(string, *) rv
    else
      string = ''
    end if
  end if

  number_to_string = trim(adjustl(string))

  return

end function number_to_string

end module hamodule
