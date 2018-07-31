module hamodule

  integer, parameter :: max_name_value = 1000
  integer, parameter :: max_string_value = 1000

  type haoptions
    logical :: definition
    character(8) :: option_name
    character(max_name_value) :: value
  end type

  interface num_len
    module procedure num_len_real_4, num_len_real_8, num_len_int_2, num_len_int_4, num_len_int_1
  end interface num_len
 
  interface number_to_string
    module procedure number_to_string_int_1, number_to_string_int_2, number_to_string_int_4,&
                     number_to_string_real_4, number_to_string_real_8
  end interface number_to_string

  interface type_select
    module procedure type_select_int_1, type_select_int_2, type_select_int_4,&
                     type_select_real_4, type_select_real_8
  end interface type_select

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

  integer function num_len_real_4(value, frmt)

    implicit none
    real(4), intent(in) :: value 
    character(*), intent(in), optional :: frmt
    character(max_string_value) :: string

    if(present(frmt)) then
      write(string, frmt) value
    else
      write(string, *) value
    end if

    num_len_real_4 = len_trim(adjustl(string))

    return

  end function num_len_real_4

  integer function num_len_real_8(value, frmt)

    implicit none
    real(8), intent(in) :: value 
    character(*), intent(in), optional :: frmt
    character(max_string_value) :: string

    if(present(frmt)) then
      write(string, frmt) value
    else
      write(string, *) value
    end if

    num_len_real_8 = len_trim(adjustl(string))

    return

  end function num_len_real_8

  integer function num_len_int_1(value, frmt)

    implicit none
    integer(1), intent(in) :: value
    character(*), intent(in), optional :: frmt
    character(max_string_value) :: string

    if(present(frmt)) then
      write(string, frmt) value
    else
      write(string, *) value
    end if

    num_len_int_1 = len_trim(adjustl(string))

    return

  end function num_len_int_1

  integer function num_len_int_2(value, frmt)

    implicit none
    integer(2), intent(in) :: value
    character(*), intent(in), optional :: frmt
    character(max_string_value) :: string

    if(present(frmt)) then
      write(string, frmt) value
    else
      write(string, *) value
    end if

    num_len_int_2 = len_trim(adjustl(string))

    return

  end function num_len_int_2

  integer function num_len_int_4(value, frmt)

    implicit none
    integer(4), intent(in) :: value
    character(*), intent(in), optional :: frmt
    character(max_string_value) :: string

    if(present(frmt)) then
      write(string, frmt) value
    else
      write(string, *) value
    end if

    num_len_int_4 = len_trim(adjustl(string))

    return

  end function num_len_int_4

  function number_to_string_real_8(value, len, frmt)

    implicit none
    real(8), intent(in) :: value
    integer(4), intent(in) :: len
    character(*), intent(in), optional :: frmt
    character(max_string_value) :: string
    character(len=len) :: number_to_string_real_8

    if(present(frmt)) then
      write(string, frmt) value
    else
      write(string, *) value
    end if

    number_to_string_real_8 = trim(adjustl(string))

    return

  end function number_to_string_real_8

  function number_to_string_real_4(value, len, frmt)

    implicit none
    real(4), intent(in) :: value
    integer(4), intent(in) :: len
    character(*), intent(in), optional :: frmt
    character(max_string_value) :: string
    character(len=len) :: number_to_string_real_4

    if(present(frmt)) then
      write(string, frmt) value
    else
      write(string, *) value
    end if

    number_to_string_real_4 = trim(adjustl(string))

    return

  end function number_to_string_real_4

  function number_to_string_int_4(value, len, frmt)

    implicit none
    integer(4), intent(in) :: value
    integer(4), intent(in) :: len
    character(*), intent(in), optional :: frmt
    character(max_string_value) :: string
    character(len=len) :: number_to_string_int_4

    if(present(frmt)) then
      write(string, frmt) value
    else
      write(string, *) value
    end if

    number_to_string_int_4 = trim(adjustl(string))

    return

  end function number_to_string_int_4

  function number_to_string_int_2(value, len, frmt)

    implicit none
    integer(2), intent(in) :: value
    integer(4), intent(in) :: len
    character(*), intent(in), optional :: frmt
    character(max_string_value) :: string
    character(len=len) :: number_to_string_int_2

    if(present(frmt)) then
      write(string, frmt) value
    else
      write(string, *) value
    end if

    number_to_string_int_2 = trim(adjustl(string))

    return

  end function number_to_string_int_2

  function number_to_string_int_1(value, len, frmt)

    implicit none
    integer(1), intent(in) :: value
    integer(4), intent(in) :: len
    character(*), intent(in), optional :: frmt
    character(max_string_value) :: string
    character(len=len) :: number_to_string_int_1

    if(present(frmt)) then
      write(string, frmt) value
    else
      write(string, *) value
    end if

    number_to_string_int_1 = trim(adjustl(string))

    return

  end function number_to_string_int_1

  integer(1) function type_select_int_1(value)
    implicit none
    integer(1), intent(in) :: value
    type_select_int_1 = value
    return
  end function type_select_int_1

  integer(2) function type_select_int_2(value)
    implicit none
    integer(2), intent(in) :: value
    type_select_int_2 = value
    return
  end function type_select_int_2

  integer(4) function type_select_int_4(value)
    implicit none
    integer(4), intent(in) :: value
    type_select_int_4 = value
    return
  end function type_select_int_4

  real(4) function type_select_real_4(value)
    implicit none
    real(4), intent(in) :: value
    type_select_real_4 = value
    return
  end function type_select_real_4

  real(8) function type_select_real_8(value)
    implicit none
    real(8), intent(in) :: value
    type_select_real_8 = value
    return
  end function type_select_real_8

end module hamodule
