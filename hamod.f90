module hamodule

use netcdf

type ncdimensions

  character(nf90_max_name) ::&
    name
  integer ::&
    dimid,&
    len

end type ncdimensions

type ncattributes
  
  character(nf90_max_name) ::&
    name
  integer ::&
    xtype,&
    len,&
    attnum 

end type ncattributes

type ncvariables

  character(nf90_max_name) ::&
    name
  integer ::&
    varid,&
    xtype,&
    ndims,&
    natts
  integer, dimension(:), allocatable ::&
    dimids
  type(ncattributes), dimension(:), allocatable :: attribute

end type ncvariables

type ncfile
  character(1000) :: path
  !character(nf90_max_name) ::& !
    !newname,& !
    !curname !
  integer ::& !
    cmode,& !
    ncid,& !
    !initialsize,& !
    !chunksize,& !
    !fillmode,& !
    !old_mode,& !
    !h_minfree,& !
    !v_align,& !
    !v_minfree,& !
    !r_align,& !
    ndimensions,& !
    nvariables,& !
    nattributes,& !
    unlimiteddimid,& !
    formatnum!,& !
    !dimid,& !
    !xtype,& !
    !size,& !
    !nfields,& !
    !ncid_in,& !
    !varid_in,& !
    !ncid_out,& !
    !varid_out !

  type(ncdimensions), dimension(:), allocatable :: dimension

  type(ncvariables), dimension(:), allocatable :: variable

  !integer, dimension(:), allocatable ::& !
    !dimids,& !
    !int_values,& !
    !start,& !
    !count,& !
    !stride,& !
    !map !

  !real(8), dimension(:), allocatable ::& !
    !real_values !

  !logical, dimension(:), allocatable ::& !
    !logical_values !

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
  integer :: i, j
  
  print '(a)', 'nc file info:'
  print '(2x,a)', 'ncid: '//&
  integer_to_string(nc_file%ncid, int_string_len(nc_file%ncid))
  print '(2x,a)', 'path: '//trim(adjustl(nc_file%path))
  print '(2x,a)', 'mode: '//&
  integer_to_string(nc_file%cmode, int_string_len(nc_file%cmode))
  print '(2x,a)', 'nDimensions: '//&
  integer_to_string(nc_file%ndimensions, int_string_len(nc_file%ndimensions))
  do i = 1, nc_file%ndimensions
    print '(4x,a)', 'dim "'//trim(nc_file%dimension(i)%name)//'": '//&
    integer_to_string(nc_file%dimension(i)%len, int_string_len(nc_file%dimension(i)%len))
  end do
  print '(2x,a)', 'nVariables: '//&
  integer_to_string(nc_file%nvariables, int_string_len(nc_file%nvariables))
  do i = 1, nc_file%nvariables
    print '(4x,a)', 'var "'//trim(nc_file%variable(i)%name)//'":'
    print '(6x,a)', 'xtype: '//&
    integer_to_string(nc_file%variable(i)%xtype, int_string_len(nc_file%variable(i)%xtype))
    print '(6x,a)', 'ndims: '//&
    integer_to_string(nc_file%variable(i)%ndims, int_string_len(nc_file%variable(i)%ndims))
    do j = 1, nc_file%variable(i)%ndims
      print '(8x,a)', 'dimid: '//&
      integer_to_string(nc_file%variable(i)%dimids(j), int_string_len(nc_file%variable(i)%dimids(j)))
    end do
    print '(6x,a)', 'natts: '//&
    integer_to_string(nc_file%variable(i)%natts, int_string_len(nc_file%variable(i)%natts))
    do j = 1, nc_file%variable(i)%natts
      print '(8x,a)', 'att "'//trim(nc_file%variable(i)%attribute(j)%name)//'":'
      print '(10x,a)', 'xtype: '//&
      integer_to_string(nc_file%variable(i)%attribute(j)%xtype,&
      int_string_len(nc_file%variable(i)%attribute(j)%xtype))
      print '(10x,a)', 'len: '//&
      integer_to_string(nc_file%variable(i)%attribute(j)%len,&
      int_string_len(nc_file%variable(i)%attribute(j)%len))

      !integer_to_s
    end do
  end do
  print '(2x,a)', 'nAttributes: '//&
  integer_to_string(nc_file%nattributes, int_string_len(nc_file%nattributes))
  print '(2x,a)', 'unlimitedDimid: '//&
  integer_to_string(nc_file%unlimiteddimid, int_string_len(nc_file%unlimiteddimid))
  print '(2x,a)', 'formatNum: '//&
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

