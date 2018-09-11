subroutine input_check(check_type, arg, string)

  implicit none

  character(*), intent(in) :: check_type
  character(*), intent(in) :: arg
  character(*), intent(in), optional :: string
  logical :: file_exist = .false., arg_true = .false.
  integer(4) :: i, k = 1, un = 6

  ! check types:
  !   'noarg' - parameter existence
  !   'nofile' - file existence
  !   'checkarg' - argument is true

  select case(check_type)
  case('noarg')
    if(trim(adjustl(arg)) == '') then
      write(un, '(a)') ''
      write(un, '(a)') 'ERROR: Do not set any option!'
      write(un, '(a)') ''
      call print_help('stop')
    end if
  case('noopt')
    if(trim(adjustl(arg)) == '') then
      write(un, '(a)') ''
      write(un, '(a)') 'ERROR: Do not define parameter of option "'//trim(adjustl(string))//'"'
      write(un, '(a)') ''
      call print_help('stop')
    end if
  case('nofile')
    inquire(file=trim(adjustl(arg)), exist=file_exist)
    if(file_exist .eqv. .false.) then
      write(un, '(a)') ''
      write(un, '(a)') 'ERROR: No such file "'//trim(adjustl(arg))//'"'
      write(un, '(a)') ''
      call print_help('stop')
    end if
  case('unopt')
    write(un, '(a)') ''
    write(un, '(a)') 'ERROR: Option "'//trim(adjustl(arg))//'" unrecognized!'
    write(un, '(a)') ''
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
        write(un, '(a)') 'ERROR: The parameter '//trim(adjustl(arg))//' is incorrect!'
        call print_help('stop')
      end if
  end select

  return

end subroutine input_check
