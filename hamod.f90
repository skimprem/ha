module hamodule

use netcdf

type ncdimensions

  character(nf90_max_name) ::&
    name
  integer ::&
    dimid,&
    len

end type ncdimensions

type ncvalues

  integer(2), dimension(:), allocatable :: short
  integer(4), dimension(:), allocatable :: int
  integer(8), dimension(:), allocatable :: int64
  real(4), dimension(:), allocatable :: float
  real(8), dimension(:), allocatable :: double
  character(nf90_max_name) :: char

end type ncvalues

type ncattributes
  
  character(nf90_max_name) ::&
    name
  integer ::&
    xtype,&
    len,&
    attnum 

  type(ncvalues) ::&
    value

end type ncattributes

type ncxtypes

  character(nf90_max_name) ::&
    name
  integer ::&
    type,&
    size

end type ncxtypes

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
  character(nf90_max_name) ::& !
    name,&
    title,&
    history,&
    conventions
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

  type(ncdimensions), dimension(:), allocatable :: dimension

  type(ncvariables), dimension(:), allocatable :: variable

  type(ncattributes), dimension(:), allocatable :: attribute

end type ncfile

contains

subroutine select_xtype(ncid, varid, xtype, name, len, value )

  use netcdf

  implicit none

  integer :: ncid, varid, xtype, len
  character(*) :: name
  type(ncvalues) :: value

    select case(input_file%variable(i)%attribute(j)%xtype)
    case(0)
      ! 0
    case(nf90_byte)
      ! NC_BYTE: 8-bit signed integer
    case(nf90_ubyte)
      ! NC_UBYTE: 8-bit unsigned integer
    case(nf90_char)
      ! NC_CHAR: 8-bit character byte
      call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = ncid,&
          varid = input_file%variable(i)%varid,&
          name = input_file%variable(i)%attribute(j)%name,&
          values = input_file%variable(i)%attribute(j)%value%char&
        )&
      )
    case(nf90_short)
      ! NC_SHORT: 16-bit signed integer
      allocate(&
        input_file%variable(i)%attribute(j)%value%short(&
          input_file%variable(i)%attribute(j)%len&
        )&
      )
      call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = input_file%ncid,&
          varid = input_file%variable(i)%varid,&
          name = input_file%variable(i)%attribute(j)%name,&
          values = input_file%variable(i)%attribute(j)%value%short&
        )&
      )
    case(nf90_ushort)
      ! NC_USHORT: 16-bit unsigned integer
    case(nf90_int)
      ! NC_INT: (NC_LONG): 32-bit signed integer
      allocate(&
        input_file%variable(i)%attribute(j)%value%int(&
          input_file%variable(i)%attribute(j)%len&
        )&
      )
      call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = input_file%ncid,&
          varid = input_file%variable(i)%varid,&
          name = input_file%variable(i)%attribute(j)%name,&
          values = input_file%variable(i)%attribute(j)%value%int&
        )&
      )
    case(nf90_uint)
      ! NC_UINT: 32-bit unsigned integer
    case(nf90_int64)
      ! NC_INT64: 64-bit signed integer
      allocate(&
        input_file%variable(i)%attribute(j)%value%int64(&
          input_file%variable(i)%attribute(j)%len&
        )&
      )
      call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = input_file%ncid,&
          varid = input_file%variable(i)%varid,&
          name = input_file%variable(i)%attribute(j)%name,&
          values = input_file%variable(i)%attribute(j)%value%int64&
        )&
      )
    case(nf90_uint64)
      ! NC_UINT64: 64-bit unsigned integer
    case(nf90_float)
      ! NC_FLOAT: 32-bit floating point
      allocate(&
        input_file%variable(i)%attribute(j)%value%float(&
          input_file%variable(i)%attribute(j)%len&
        )&
      )
      call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = input_file%ncid,&
          varid = input_file%variable(i)%varid,&
          name = input_file%variable(i)%attribute(j)%name,&
          values = input_file%variable(i)%attribute(j)%value%float&
        )&
      )
    case(nf90_double)
      ! NC_DOUBLE: 64-bit floating point
      allocate(&
        input_file%variable(i)%attribute(j)%value%double(&
          input_file%variable(i)%attribute(j)%len&
        )&
      )
      call nc_error_check(&
        'nc_get_att',&
        nf90_get_att(&
          ncid = input_file%ncid,&
          varid = input_file%variable(i)%varid,&
          name = input_file%variable(i)%attribute(j)%name,&
          values = input_file%variable(i)%attribute(j)%value%double&
        )&
      )
    case(nf90_string)
      ! NC_STRING: variable length character string
    end select

end subroutine select_xtype

subroutine nc_error_check(check_type, ncstatus)

  use netcdf

  implicit none
  
  character(*), intent(in) :: check_type
  integer, intent(in) :: ncstatus

  if(ncstatus /= nf90_noerr) then
    select case(check_type)
    case('nc_open')
      print '(a)',&
      'Error in nc_open: '//&
      number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
    case('nc_inquire')
      print '(a)',&
      'Error in nc_inquire: '//&
      number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
    case('nc_inq_dimid')
      print '(a)',&
      'Error in nc_inq_dimid: '//&
      number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
    case('nc_inquire_dimension')
      print '(a)',&
      'Error in nc_inquire_dimension: '//&
      number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
    case('nc_inq_varid')
      print '(a)',&
      'Error in nc_inq_varid: '//&
      number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
    case('nc_variable')
      print '(a)',&
      'Error in nc_variable: '//&
      number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
    case('nc_inq_attname')
      print '(a)',&
      'Error in nc_inq_attname: '//&
      number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
    case('nc_inquire_attribute')
      print '(a)',&
      'Error in nc_inquire_attribute: '//&
      number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
    case('nc_get_att')
      print '(a)',&
      'Error in nc_get_att: '//&
      number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
    case('nc_')
      print '(a)',&
      'Error in nc_: '//&
      number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))

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
  integer :: i, j, k
  
  print '(a)', 'NetCDF version: '//trim(nf90_inq_libvers())
  print '(a)', 'nc file info:'
  print '(2x,a)', 'ncid: '//&
  number_to_string(iv = int(nc_file%ncid, 4),&
  len = num_len(iv = int(nc_file%ncid, 4)))
  print '(2x,a)', 'path: '//trim(adjustl(nc_file%path))
  print '(2x,a)', 'mode: '//&
  number_to_string(iv = int(nc_file%cmode, 4),&
  len = num_len(int(nc_file%cmode, 4)))
  print '(2x,a)', 'nDimensions: '//&
  number_to_string(iv = int(nc_file%ndimensions, 4),&
  len = num_len(int(nc_file%ndimensions, 4)))
  do i = 1, nc_file%ndimensions
    print '(4x,a)', 'name: '//trim(nc_file%dimension(i)%name)
    print '(6x,a)', 'len: '//&
    number_to_string(iv = int(nc_file%dimension(i)%len, 4),&
    len = num_len(iv = int(nc_file%dimension(i)%len,4)))
  end do
  print '(2x,a)', 'nVariables: '//&
  number_to_string(iv = int(nc_file%nvariables, 4),&
  len = num_len(iv = int(nc_file%nvariables, 4)))
  do i = 1, nc_file%nvariables
    print '(4x,a)', 'name: '//trim(nc_file%variable(i)%name)
    print '(6x,a)', 'xtype: '//&
    number_to_string(iv = int(nc_file%variable(i)%xtype, 4),&
    len = num_len(iv = int(nc_file%variable(i)%xtype, 4)))
    print '(6x,a)', 'ndims: '//&
    number_to_string(iv = int(nc_file%variable(i)%ndims, 4),&
    len = num_len(iv = int(nc_file%variable(i)%ndims, 4)))
    do j = 1, nc_file%variable(i)%ndims
      print '(8x,a)', 'dimid: '//&
      number_to_string(iv = int(nc_file%variable(i)%dimids(j), 4),&
      len = num_len(iv = int(nc_file%variable(i)%dimids(j), 4)))
    end do
    print '(6x,a)', 'natts: '//&
    number_to_string(iv = int(nc_file%variable(i)%natts, 4),&
    len = num_len(iv = int(nc_file%variable(i)%natts, 4)))
    do j = 1, nc_file%variable(i)%natts
      print '(8x,a)', 'name: '//&
      trim(nc_file%variable(i)%attribute(j)%name)
      print '(10x,a)', 'xtype: '//&
      number_to_string(iv = int(nc_file%variable(i)%attribute(j)%xtype, 4),&
      len = num_len(iv = int(nc_file%variable(i)%attribute(j)%xtype, 4)))
      print '(10x,a)', 'len: '//&
      number_to_string(iv = int(nc_file%variable(i)%attribute(j)%len, 4),&
      len = num_len(iv = int(nc_file%variable(i)%attribute(j)%len, 4)))
    
      select case(nc_file%variable(i)%attribute(j)%xtype)
      case(nf90_float)
        do k = 1, nc_file%variable(i)%attribute(j)%len
          print '(10x,a)',&
          'value '//&
          number_to_string(iv = int(k, 4), len = num_len(iv = int(k, 4)))//': '//&
          number_to_string(rv = real(nc_file%variable(i)%attribute(j)%value%float(k), 4),&
          len = num_len(rv = real(nc_file%variable(i)%attribute(j)%value%float(k), 4)))!,&
          !frmt = '(f10.3)')
        end do
      case(nf90_double)
        do k = 1, nc_file%variable(i)%attribute(j)%len
          print '(10x,a)',&
          'value '//&
          number_to_string(iv = int(k, 4), len = num_len(iv = int(k, 4)))//': '//&
          number_to_string(rv = real(nc_file%variable(i)%attribute(j)%value%double(k), 4),&
          len = num_len(rv = real(nc_file%variable(i)%attribute(j)%value%double(k), 4)))!,&
          !frmt = '(f100.3)')
        end do
      case(nf90_char)
        print '(10x,a)', 'value: '//&
        trim(nc_file%variable(i)%attribute(j)%value%char)
      end select
    end do
  end do
  print '(2x,a)', 'nAttributes: '//&
  number_to_string(iv = int(nc_file%nattributes, 4),&
  len = num_len(iv = int(nc_file%nattributes, 4)))
  print '(4x,a)', 'title: '//trim(nc_file%title)
  print '(2x,a)', 'unlimitedDimid: '//&
  number_to_string(iv = int(nc_file%unlimiteddimid, 4),&
  len = num_len(iv = int(nc_file%unlimiteddimid, 4)))
  print '(2x,a)', 'formatNum: '//&
  number_to_string(iv = int(nc_file%formatnum, 4),&
  len = num_len(iv = int(nc_file%formatnum, 4)))

end subroutine print_nc_info

integer(4) function num_len(iv, rv)

  implicit none
  integer(4), intent(in), optional :: iv
  real, intent(in), optional :: rv
  character(10000) :: string

  if(present(iv)) then
    write(string, *) iv
  else if(present(rv)) then
    write(string, *) rv
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

