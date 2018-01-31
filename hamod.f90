module hamodule

use netcdf

type ncinquire
  integer :: ncid, ndimensions, nvariables, nattributes, unlimiteddimid, formatnum,&
  dimid, len
  character(len=nf90_max_name) :: name
end type ncinquire

type ncfile
  character(500) :: path
  integer :: cmode, ncid, initialsize, chunksize
  type(ncinquire) :: inquire
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

subroutine print_nc_info(nc_file, type_info)
  implicit none
  type(ncfile), intent(in) :: nc_file
  character(*), intent(in), optional :: type_info
  
  print '(a)', 'ncid: '//&
  integer_to_string(nc_file%ncid, int_string_len(nc_file%ncid))
  print '(a)', 'path: '//trim(adjustl(nc_file%path))
  print '(a)', 'mode: '//&
  integer_to_string(nc_file%cmode, int_string_len(nc_file%cmode))
  print '(a)', 'nDimensions: '//&
  integer_to_string(nc_file%inquire%ndimensions, int_string_len(nc_file%inquire%ndimensions))
  print '(a)', 'nVariables: '//&
  integer_to_string(nc_file%inquire%nvariables, int_string_len(nc_file%inquire%nvariables))
  print '(a)', 'nAttributes: '//&
  integer_to_string(nc_file%inquire%nattributes, int_string_len(nc_file%inquire%nattributes))
  print '(a)', 'unlimitedDimid: '//&
  integer_to_string(nc_file%inquire%unlimiteddimid, int_string_len(nc_file%inquire%unlimiteddimid))
  print '(a)', 'formatNum: '//&
  integer_to_string(nc_file%inquire%formatnum, int_string_len(nc_file%inquire%formatnum))
  print '(a)', 'dimid: '//&
  integer_to_string(nc_file%inquire%dimid, int_string_len(nc_file%inquire%dimid))
  print '(a)', 'name: '//trim(adjustl(nc_file%inquire%name))
  print '(a)', 'len: '//&
  integer_to_string(nc_file%inquire%len, int_string_len(nc_file%inquire%len))


end subroutine print_nc_info

integer(4) function int_string_len(val)

  implicit none
  integer(4), intent(in) :: val
  character(10000) :: string

  write(string, *) val

  int_string_len = len_trim(adjustl(string))

  return

end function int_string_len

function integer_to_string(val, string_len, frmt)

  implicit none
  integer(4), intent(in) :: val
  integer(4), intent(in) :: string_len
  character(*), intent(in), optional :: frmt
  character(10000) :: string
  character(len=string_len) :: integer_to_string
  
  if(present(frmt)) then
      write(string, frmt) val
  else
      write(string, *) val
  end if

  integer_to_string = trim(adjustl(string))

  return

end function integer_to_string

end module hamodule

