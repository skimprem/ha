module hamodule

  integer, parameter :: max_name_value = 1000

  type haoptions
    logical :: definition
    character(8) :: option_name
    character(max_name_value) :: value
  end type

contains

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

  subroutine print_help(type_help)
    implicit none
    character(*), intent(in), optional :: type_help

    !print '(a)', 'Usage: ha [OPTION]... [FILE]...'
    print '(a)', 'Usage: ha [OPTION]...'
    print '(a)', ''
    print '(a)', 'Calculates the coefficients of spherical harmonics over a regular grid' 
    print '(a)', 'using various algorithms'
    print '(a)', ''
    print '(a)', '  -nf, --ncfile  set the source file with grid data in NetCDF format'
    print '(a)', ''
    print '(a)', '  -nm, --ncmode  set the output mode for the source file with grid data using the following parameters:'
    print '(a)', "                 'view' - print the information about the grid file along with the data"
    print '(a)', "                 'viewdata' - print only data"
    print '(a)', "                 'viewinfo' - print only information about the grid file"
    print '(a)', ''
    print '(a)', '  -hm, --hamode  set expand method using the following parameters:'
    print '(a)', "                 'dh' - Driscoll and Healy sampling theorem"
    print '(a)', "                 'ls' - Least squares inversion"
    print '(a)', ''
    print '(a)', '  -h, --help     print help'
    print '(a)', ''



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

  function nc_value_to_string(&!
      byte,&!
      ubyte,&!
      char,&!
      short,&!
      ushort,&!
      int,&!
      uint,&!
      int64,&!
      uint64,&!
      float,&!
      double,&!
      string)!
      !xtype)
    
    implicit none

    integer(kind=1), intent(in), optional :: byte
    uinteger(kind=1), intent(in), optional :: ubyte
    character(kind=1, len=1), intent(in), optional :: char
    integer(kind=2), intent(in), optional :: short
    uinteger(kind=2), intent(in), optional :: ushort
    integer(kind=4), intent(in), optional :: int
    uinteger(kind=4), intent(in), optional :: uint
    integer(kind=8), intent(in), optional :: int64
    uinteger(kind=8), intent(in), optional :: uint64
    real(kind=4), intent(in), optional :: float
    real(kind=8), intent(in), optional :: double
    character(kind=1, len=nf90_max_name), intent(in), optional :: string
    !integer(kind=4), intent(in) :: xtype

    integer(kind=4) :: i,&!
                       value_len, all_value_number = 12,&!
                       max_value_number = 1,&!
                       present_value_number

    character(kind=1, len=nf90_max_name) :: nc_value_to_string

    i = 0

    if(present(byte)) i = i + 1
    if(present(ubyte)) i = i + 1
    if(present(char)) i = i + 1
    if(present(short)) i = i + 1
    if(present(ushort)) i = i + 1
    if(present(int)) i = i + 1
    if(present(uint)) i = i + 1
    if(present(int64)) i = i + 1
    if(present(uint64)) i = i + 1
    if(present(float)) i = i + 1
    if(present(double)) i = i + 1
    if(present(string)) i = i + 1

    if(i > 1) stop 'i > 1'
    if(i < 1) stop 'i < 1'

    select case(xtype)
    case(nf90_byte)
      nc_xtype_name = 'byte: 8-bit signed integer'
    case(nf90_ubyte)
      nc_xtype_name = 'ubyte: 8-bit unsigned integer' 
    case(nf90_char)
      nc_xtype_name = 'char: 8-bit character byte'
    case(nf90_short)
      nc_xtype_name = 'short: 16-bit signed integer'
    case(nf90_ushort)
      nc_xtype_name = 'ushort: 16-bit unsigned integer'
    case(nf90_int)
      nc_xtype_name = 'int: 32-bit signed integer'
    case(nf90_uint)
      nc_xtype_name = 'uint: 32-bit unsigned integer'
    case(nf90_int64)
      nc_xtype_name = 'int64: 64-bit signed integer'
    case(nf90_uint64)
      nc_xtype_name = 'uint64: 64-bit signed integer'
    case(nf90_float)
      nc_xtype_name = 'float: 32-bit floating point'
    case(nf90_double)
      nc_xtype_name = 'double: 64-bit floating point'
    case(nf90_string)
      nc_xtype_name = 'string: variable length character string'
    end select

    return

  end function nc_value_to_string`
end module hamodule
