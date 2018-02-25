module hamodule

use netcdf

type two_dimension_len
  integer(4) :: nx, ny
end type

type ncdimensions

  character(nf90_max_name) ::&
    name
  integer ::&
    dimid,&
    len

end type ncdimensions

type ncvalues

  integer(2) :: short
  integer(4) :: int
  integer(8) :: int64
  real(4) :: float
  real(8) :: double
  character(nf90_max_name) :: char

end type ncvalues

type ncattributes
  
  character(nf90_max_name) ::&
    name
  integer ::&
    xtype,&
    len,&
    attnum 

  type(ncvalues), dimension(:), allocatable ::&
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
  integer, dimension(:), allocatable ::&
    len
  type(ncattributes), dimension(:), allocatable :: attribute
  type(ncvalues), dimension(:), allocatable :: val1
  type(ncvalues), dimension(:,:), allocatable :: val2
  type(ncvalues), dimension(:,:,:), allocatable :: val3

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

subroutine get_att_xtype( ncid, varid, xtype, name, len, value )

  use netcdf

  implicit none

  integer, intent(in) :: ncid, varid, xtype, len
  character(*), intent(in) :: name
  type(ncvalues), intent(out), dimension(:), allocatable :: value

  allocate( value(len) )

  select case(xtype)
  case(0)
    ! 0
  case(nf90_byte)
    ! NC_BYTE: 8-bit signed integer
  case(nf90_ubyte)
    ! NC_UBYTE: 8-bit unsigned integer
  case(nf90_char)
    ! NC_CHAR: 8-bit character byte
    deallocate( value )
    allocate( value(1) )
    call nc_error_check(&
      'nc_get_att',&
      nf90_get_att(&
        ncid = ncid,&
        varid = varid,&
        name = name,&
        values = value(1)%char&
      )&
    )
  case(nf90_short)
    ! NC_SHORT: 16-bit signed integer
    call nc_error_check(&
      'nc_get_att',&
      nf90_get_att(&
        ncid = ncid,&
        varid = varid,&
        name = name,&
        values = value%short&
      )&
    )
  case(nf90_ushort)
    ! NC_USHORT: 16-bit unsigned integer
  case(nf90_int)
    ! NC_INT: (NC_LONG): 32-bit signed integer
    call nc_error_check(&
      'nc_get_att',&
      nf90_get_att(&
        ncid = ncid,&
        varid = varid,&
        name = name,&
        values = value%int&
      )&
    )
  case(nf90_uint)
    ! NC_UINT: 32-bit unsigned integer
  case(nf90_int64)
    ! NC_INT64: 64-bit signed integer
    call nc_error_check(&
      'nc_get_att',&
      nf90_get_att(&
        ncid = ncid,&
        varid = varid,&
        name = name,&
        values = value%int64&
      )&
    )
  case(nf90_uint64)
    ! NC_UINT64: 64-bit unsigned integer
  case(nf90_float)
    ! NC_FLOAT: 32-bit floating point
    call nc_error_check(&
      'nc_get_att',&
      nf90_get_att(&
        ncid = ncid,&
        varid = varid,&
        name = name,&
        values = value%float&
      )&
    )
  case(nf90_double)
    ! NC_DOUBLE: 64-bit floating point
    call nc_error_check(&
      'nc_get_att',&
      nf90_get_att(&
        ncid = ncid,&
        varid = varid,&
        name = name,&
        values = value%double&
      )&
    )
  case(nf90_string)
    ! NC_STRING: variable length character string
  end select

end subroutine get_att_xtype

subroutine get_var_xtype(&
  ncid,&
  varid,&
  xtype,&
  ndims,&
  len,&
  val1,&
  val2,&
  val3)

  implicit none
  integer, intent(in) :: ncid, varid, xtype, ndims
  integer, intent(in), dimension(ndims) :: len
  type(ncvalues), intent(out), dimension(:), allocatable, optional :: val1
  type(ncvalues), intent(out), dimension(:,:), allocatable, optional :: val2
  type(ncvalues), intent(out), dimension(:,:,:), allocatable, optional :: val3
  !type(ncvalues), intent(out), dimension(:,:,:,:), allocatable, optional :: val4
  !type(ncvalues), intent(out), dimension(:,:,:,:,:), allocatable, optional :: val5

  select case(ndims)
  case(1)
    allocate( val1(len(1)) )
  case(2)
    allocate( val2(len(1), len(2)) )
  case(3)
    allocate( val3(len(1), len(2), len(3)) )
  end select

  select case(xtype)
  case(nf90_byte)
  case(nf90_ubyte)
  case(nf90_char)
  case(nf90_short)
  case(nf90_ushort)
  case(nf90_int)
  case(nf90_uint)
  case(nf90_int64)
  case(nf90_uint64)
  case(nf90_float)
    select case(ndims)
    case(1)
      call nc_error_check(&
        'nc_get_var',&
        nf90_get_var(&
          ncid = ncid,&
          varid = varid,&
          values = val1%float&
        )&
      )
    case(2)
      call nc_error_check(&
        'nc_get_var',&
        nf90_get_var(&
          ncid = ncid,&
          varid = varid,&
          values = val2%float&
        )&
      )
    case(3)
      call nc_error_check(&
        'nc_get_var',&
        nf90_get_var(&
          ncid = ncid,&
          varid = varid,&
          values = val3%float&
        )&
      )

    end select
  case(nf90_double)
    select case(ndims)
    case(1)
      call nc_error_check(&
        'nc_get_var',&
        nf90_get_var(&
          ncid = ncid,&
          varid = varid,&
          values = val1%double&
        )&
      )
    case(2)
      call nc_error_check(&
        'nc_get_var',&
        nf90_get_var(&
          ncid = ncid,&
          varid = varid,&
          values = val2%double&
        )&
      )
    case(3)
      call nc_error_check(&
        'nc_get_var',&
        nf90_get_var(&
          ncid = ncid,&
          varid = varid,&
          values = val3%double&
        )&
      )
    end select
  case(nf90_string)
  end select

end subroutine get_var_xtype

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
  logical :: file_exist = .false., arg_true = .false.
  integer(4) :: i, k = 1

  ! check types:
  !   'noarg' - parameter existence
  !   'nofile' - file existence
  !   'checkarg' - argument is true

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
  case('checkarg')
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

  print '(a)', 'print help'

  if(present(type_help)) then
    select case(type_help)
    case('stop')
      !stop 'Stopped!'
      stop
    end select
  end if

  return
end subroutine print_help

subroutine print_nc_info(nc_file, type_info)
  implicit none
  type(ncfile), intent(in) :: nc_file
  character(*), intent(in), optional :: type_info
  integer :: i, j, k, l, m
  
  ! type_info:
  !   view
  !   viewdata
  !   viewinfo

  if(type_info == 'viewinfo' .or. type_info == 'view') then
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
            number_to_string(rv = real(nc_file%variable(i)%attribute(j)%value(k)%float, 4),&
            len = num_len(rv = real(nc_file%variable(i)%attribute(j)%value(k)%float, 4)))!,&
            !frmt = '(f10.3)')
          end do
        case(nf90_double)
          do k = 1, nc_file%variable(i)%attribute(j)%len
            print '(10x,a)',&
            'value '//&
            number_to_string(iv = int(k, 4), len = num_len(iv = int(k, 4)))//': '//&
            number_to_string(rv = real(nc_file%variable(i)%attribute(j)%value(k)%double, 4),&
            len = num_len(rv = real(nc_file%variable(i)%attribute(j)%value(k)%double, 4)))!,&
            !frmt = '(f100.3)')
          end do
        case(nf90_char)
          print '(10x,a)', 'value: '//&
          trim(nc_file%variable(i)%attribute(j)%value(1)%char)
        end select
      end do
     end do
    print '(2x,a)', 'nAttributes: '//&
    number_to_string(iv = int(nc_file%nattributes, 4),&
    len = num_len(iv = int(nc_file%nattributes, 4)))
    do i = 1, nc_file%nattributes
      print '(4x,a)', trim(nc_file%attribute(i)%name)//': '//&
      trim(nc_file%attribute(i)%value(1)%char)
    end do
    print '(2x,a)', 'unlimitedDimid: '//&
    number_to_string(iv = int(nc_file%unlimiteddimid, 4),&
    len = num_len(iv = int(nc_file%unlimiteddimid, 4)))
    print '(2x,a)', 'formatNum: '//&
    number_to_string(iv = int(nc_file%formatnum, 4),&
    len = num_len(iv = int(nc_file%formatnum, 4)))
  end if

  if(type_info == 'viewdata' .or. type_info == 'view') then
    print *, ''
    do k = 1, nc_file%nvariables
      do m = 1, nc_file%variable(k)%ndims
        do i = 1, nc_file%variable(k)%len(m)
          do l = 1, nc_file%variable(k+1)%ndims
            do j = 1, nc_file%variable(k+1)%len(l)
              print '(a)', &
              number_to_string(rv = real(nc_file%variable(k)%val1(j)%double, 4),&
              len = num_len(rv = real(nc_file%variable(k)%val1(j)%double, 4),&
              frmt = '(f20.2)'),&
              frmt = '(f20.2)')//' '//&
              number_to_string(rv = real(nc_file%variable(k+1)%val1(i)%double, 4),&
              len = num_len(rv = real(nc_file%variable(k+1)%val1(i)%double, 4),&
              frmt = '(f20.2)'),&
              frmt = '(f20.2)')//' '//&
              number_to_string(rv = real(nc_file%variable(k+2)%val2(j,i)%float, 4),&
              len = num_len(rv = real(nc_file%variable(k+2)%val2(j,i)%float, 4),&
              frmt = '(f20.4)'),&
              frmt = '(f20.4)')
            end do
          end do
        end do
      end do
    end do
  end if

  return

end subroutine print_nc_info

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

subroutine nc_reader(input_file, mode)

  use netcdf

  implicit none
  type(ncfile), intent(inout) :: input_file
  character(*), intent(in), optional :: mode
  integer :: k = 0, i, j, ncstatus

  input_file%cmode = nf90_nowrite

  call nc_error_check(&
    'nc_open',&
    nf90_open(&
      path = input_file%path,&
      mode = input_file%cmode,&
      ncid = input_file%ncid&
    )&
  )

  call nc_error_check(&
    'nc_inquire',&
    nf90_inquire(&
      ncid = input_file%ncid,&
      ndimensions = input_file%ndimensions,&
      nvariables = input_file%nvariables,&
      nattributes = input_file%nattributes,&
      unlimiteddimid = input_file%unlimiteddimid,&
      formatnum = input_file%formatnum&
    )&
  )

  allocate(&
    input_file%dimension(input_file%ndimensions),&
    input_file%variable(input_file%nvariables),&
    input_file%attribute(input_file%nattributes)&
  )

  do i = 1, input_file%nattributes
    
    input_file%attribute(i)%attnum = i

    call nc_error_check(&
      'nc_inq_attname',&
      nf90_inq_attname(&
        ncid = input_file%ncid,&
        varid = nf90_global,&
        attnum = input_file%attribute(i)%attnum,&
        name = input_file%attribute(i)%name&
      )&
    )

    call nc_error_check(&
      'nc_inquire_attribute',&
      nf90_inquire_attribute(&
        ncid = input_file%ncid,&
        varid = nf90_global,&
        name = input_file%attribute(i)%name,&
        xtype = input_file%attribute(i)%xtype,&
        len = input_file%attribute(i)%len&
      )&
    )

    call get_att_xtype(&
      ncid = input_file%ncid,&
      varid = nf90_global,&
      xtype = input_file%attribute(i)%xtype,&
      name = input_file%attribute(i)%name,&
      len = input_file%attribute(i)%len,&
      value = input_file%attribute(i)%value&
    )

  end do
  
  do i = 1, input_file%ndimensions

    input_file%dimension(i)%dimid = i

    call nc_error_check(&
      'nc_inquire_dimension',&
      nf90_inquire_dimension(&
        ncid = input_file%ncid,&
        dimid = input_file%dimension(i)%dimid,&
        name = input_file%dimension(i)%name,&
        len = input_file%dimension(i)%len&
      )&
    )

  end do

  do i = 1, input_file%nvariables
    
    input_file%variable(i)%varid = i

    call nc_error_check(&
      'nc_inquire_variable',&
      nf90_inquire_variable(&
        ncid = input_file%ncid,&
        varid = input_file%variable(i)%varid,&
        name = input_file%variable(i)%name,&
        xtype = input_file%variable(i)%xtype,&
        ndims = input_file%variable(i)%ndims,&
        natts = input_file%variable(i)%natts&
      )&
    )
    
    allocate(&
      input_file%variable(i)%dimids(input_file%variable(i)%ndims),&
      input_file%variable(i)%attribute(input_file%variable(i)%natts)&
    )

    call nc_error_check(&
      'nc_inquire_variable',&
      nf90_inquire_variable(&
        ncid = input_file%ncid,&
        varid = input_file%variable(i)%varid,&
        dimids = input_file%variable(i)%dimids&
      )&
    )

    do j = 1, input_file%variable(i)%natts
      
      input_file%variable(i)%attribute(j)%attnum = j

      call nc_error_check(&
        'nc_inq_attname',&
        nf90_inq_attname(&
          ncid = input_file%ncid,&
          varid = input_file%variable(i)%varid,&
          attnum = input_file%variable(i)%attribute(j)%attnum,&
          name = input_file%variable(i)%attribute(j)%name&
        )&
      )
      
      call nc_error_check(&
        'nc_inquire_attribute',&
        nf90_inquire_attribute(&
          ncid = input_file%ncid,&
          varid = input_file%variable(i)%varid,&
          name = input_file%variable(i)%attribute(j)%name,&
          xtype = input_file%variable(i)%attribute(j)%xtype,&
          len = input_file%variable(i)%attribute(j)%len&
        )&
      )

      call get_att_xtype(&
        input_file%ncid,&
        input_file%variable(i)%varid,&
        input_file%variable(i)%attribute(j)%xtype,&
        input_file%variable(i)%attribute(j)%name,&
        input_file%variable(i)%attribute(j)%len,&
        input_file%variable(i)%attribute(j)%value&
      )

    end do

    allocate( input_file%variable(i)%len(input_file%variable(i)%ndims) )
    
    do j = 1, input_file%variable(i)%ndims
      call nc_error_check(&
        'nc_inquire_dimension',&
        nf90_inquire_dimension(&
          ncid = input_file%ncid,&
          dimid = input_file%variable(i)%dimids(j),&
          len = input_file%variable(i)%len(j)&
        )&
      )
    end do


    select case(input_file%variable(i)%ndims)
    case(1)
      call get_var_xtype(&
        ncid = input_file%ncid,&
        varid = input_file%variable(i)%varid,&
        xtype = input_file%variable(i)%xtype,&
        ndims = input_file%variable(i)%ndims,&
        len = input_file%variable(i)%len,&
        val1 = input_file%variable(i)%val1&
      )
    case(2)
      call get_var_xtype(&
        ncid = input_file%ncid,&
        varid = input_file%variable(i)%varid,&
        xtype = input_file%variable(i)%xtype,&
        ndims = input_file%variable(i)%ndims,&
        len = input_file%variable(i)%len,&
        val2 = input_file%variable(i)%val2&
      )
    end select

  end do
 
  if(present(mode)) call print_nc_info(input_file, mode)

end subroutine nc_reader

end module hamodule

