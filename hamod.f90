module hamodule

use netcdf

type ncfile
  character(1000) :: path
  character(nf90_max_name) ::& !
             name,& !
             newname,& !
             curname !
  integer ::& !
             cmode,& !
             ncid,& !
             initialsize,& !
             chunksize,& !
             fillmode,& !
             old_mode,& !
             h_minfree,& !
             v_align,& !
             v_minfree,& !
             r_align,& !
             ndimensions,& !
             nvariables,& !
             nattributes,& !
             unlimiteddimid,& !
             formatnum,& !
             len,& !
             dimid,& !
             xtype,& !
             varid,& !
             ndims,& !
             natts,& !
             attnum,& !
             ncid_in,& !
             varid_in,& !
             ncid_out,& !
             varid_out !

  integer, dimension(:), allocatable ::& !
             dimids,& !
             int_values,& !
             start,& !
             count,& !
             stride,& !
             map !

  real(8), dimension(:), allocatable ::& !
             real_values !

  logical, dimension(:), allocatable ::& !
             logical_values !

end type ncfile

contains

subroutine nc_error_check(check_type, ncstatus)

  use netcdf

  implicit none
  
  character(*), intent(in) :: check_type
  integer, intent(in) :: ncstatus

  if(ncstatus /= nf90_noerr) then
    select case(check_type)
    case('nc_open')
      print '(a)',&
      'Error in nc_open: '//integer_to_string(ncstatus, int_string_len(ncstatus))
    case('nc_inquire')
      print '(a)',&
      'Error in nc_inquire: '//integer_to_string(ncstatus, int_string_len(ncstatus))
    case('nc_inq_dimid')
      print '(a)',&
      'Error in nc_inq_dimid: '//integer_to_string(ncstatus, int_string_len(ncstatus))
    case('nc_inquire_dimension')
      print '(a)',&
      'Error in nc_inquire_dimension: '//integer_to_string(ncstatus, int_string_len(ncstatus))
    case('nc_inq_varid')
      print '(a)',&
      'Error in nc_inq_varid: '//integer_to_string(ncstatus, int_string_len(ncstatus))
    end select
    print '(a)', trim(nf90_strerror(ncstatus))
    stop 'Stopped!'
  end if

  return

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
  
  print '(a)', 'nc file info:'
  print '(1x,a)', 'ncid: '//&
  integer_to_string(nc_file%ncid, int_string_len(nc_file%ncid))
  print '(1x,a)', 'path: '//trim(adjustl(nc_file%path))
  print '(1x,a)', 'mode: '//&
  integer_to_string(nc_file%cmode, int_string_len(nc_file%cmode))
  print '(1x,a)', 'nDimensions: '//&
  integer_to_string(nc_file%ndimensions, int_string_len(nc_file%ndimensions))
  print '(1x,a)', 'nVariables: '//&
  integer_to_string(nc_file%nvariables, int_string_len(nc_file%nvariables))
  print '(1x,a)', 'nAttributes: '//&
  integer_to_string(nc_file%nattributes, int_string_len(nc_file%nattributes))
  print '(1x,a)', 'unlimitedDimid: '//&
  integer_to_string(nc_file%unlimiteddimid, int_string_len(nc_file%unlimiteddimid))
  print '(1x,a)', 'formatNum: '//&
  integer_to_string(nc_file%formatnum, int_string_len(nc_file%formatnum))
  !print '(1x,a)', 'dimid: '//&
  !integer_to_string(nc_file%dimid, int_string_len(nc_file%dimid))
  !print '(1x,a)', 'name: '//trim(adjustl(nc_file%name))
  !print '(1x,a)', 'len: '//&
  !integer_to_string(nc_file%len, int_string_len(nc_file%len))
  !print '(1x,a)', 'varid: '//&
  !integer_to_string(nc_file%varid, int_string_len(nc_file%varid))

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

