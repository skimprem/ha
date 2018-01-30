module hamodule

type ncinquire
  integer :: ncid, ndimensions, nvariables, nattributes, unlimiteddimid, formatnum
end type ncinquire

type ncfile
  character(500) :: path
  integer :: cmode, ncid, initialsize, chunksize
  type(ncinquire) :: inq
end type ncfile

contains

subroutine nc_error_check(check_type, ncstatus, string)

  implicit none
  
  character(*), intent(in) :: check_type
  integer, intent(in) :: ncstatus
  character(*), intent(in), optional :: string

  select case(check_type)
  case('nc_open')
    select case(ncstatus)
    case(2)
      print '(a)', 'ERROR: File '//trim(adjustl(string))//' do not exist!'
      stop 'Stopped!'
    end select
  case('nc_inquire')
    select case(ncstatus)
    case(2)
      stop 'Stopped!'
    end select
  end select

end subroutine nc_error_check

subroutine input_check(check_type, arg, string)

  implicit none

  character(*), intent(in) :: check_type
  character(*), intent(in) :: arg
  character(*), intent(in), optional :: string
  logical :: file_exist = .false.

  ! check types:
  !   'noarg' - parameter existence
  !   'nofile' - file existence
  !   

  select case(check_type)
  case('noarg')
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
  case('noopt')
    print '(a)', 'ERROR: Option "'//trim(adjustl(arg))//'" unrecognized!'
    call print_help('stop')
  end select

  return

end subroutine input_check

subroutine print_help(type_help)
  implicit none
  character(*), intent(in), optional :: type_help

  print '(a)', 'SOS!'

  if(present(type_help)) then
    select case(type_help)
    case('stop')
      stop 'Stopped!'
    end select
  end if

  return
end subroutine print_help

end module hamodule

