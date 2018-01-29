program ha

use netcdf

  implicit none

  character(*), parameter :: version = '1.0'
  character(*) :: arg*500, ncfile*250
  integer(4) :: j
  integer :: ncun, ncstatus
  integer :: ndim, nvar, natt, unlimdim, fmtnum

  arg = ''

  do while(j < command_argument_count())
      j = j + 1
      call get_command_argument(j, arg)

      select case(arg)
      case('-nf', '--ncfile')
        j = j + 1
        call get_command_argument(j, arg)
        call input_check(1, arg, '-nf' )
        ncfile = adjustl(arg)
        print '(a)', 'input ncfile = '//trim(adjustl(ncfile))
      end select
  end do

  ncstatus = nf90_open(path = ncfile, mode = nf90_nowrite, ncid = ncun)
  if(ncstatus /= nf90_noerr) call nc_error_check(ncstatus, ncfile)
  ncstatus = nf90_inquire(ncun, ndimensions = ndim, nvariables = nvar, &
    nattributes = natt, unlimiteddimid = unlimdim, formatnum = fmtnum)

  print *, 'nDimensions = ', ndim
  print *, 'nvariables = ', nvar 
  print *, 'nAttributes = ', natt
  print *, 'unlimitedDimid = ', unlimdim
  print *, 'formatNum = ', fmtnum

  !ncstatus = nf90_inquire_dimension(ncun, dimid = 

contains

subroutine nc_error_check(ncstatus, string)
  implicit none

  integer, intent(in) :: ncstatus
  character(*), intent(in) :: string

  select case(ncstatus)
  case(2)
    print '(a)', 'ERROR: File '//trim(adjustl(string))//' do not exist!'
  end select
end subroutine nc_error_check

subroutine input_check(var, arg, string)
  implicit none
  integer, intent(in) :: var
  character(*), intent(in) :: arg
  character(*), intent(in), optional :: string
  logical :: file_exist = .false.
  ! check variant:
  !   1 - parameter existence
  !   2 - file existence
  !   
  select case(var)
  case(1)
    if(blank_string(arg)) then
      call print_error(5, trim(adjustl(string))) 
    end if
  case(2)
    inquire(file=trim(adjustl(arg)), exist=file_exist)
    if(file_exist .eqv. .false.) then
      call print_error(7, trim(adjustl(arg)))
    end if
  end select
end subroutine input_check

function blank_string(string)
  implicit none
  logical :: blank_string
  character(*), intent(in) :: string
  blank_string = .false. 
  if (trim(adjustl(string)) == '') then
    blank_string = .true. 
  end if
end function

subroutine print_error(i, arg)
  ! i-parameter
  ! 1: unrecognized option -> stop
  ! 2: do not set option 'arg' -> stop
  ! 3: do not set necessary option -> stop
  ! 4: unrecognized parameter 'arg' -> stop
  ! 5: do not set parameter 'arg' -> stop
  ! 6: do not set necessary parameter -> stop
  ! 7: file not exist -> stop
  ! 8: file not exist -> return
  ! 9: unrecognized parameter 'arg' -> return
  implicit none
  character(*), optional :: arg
  integer, optional :: i
  !print '(/)'
  if (present(i)) then
    select case(i)
    case (1)
      print '(a,a,a,/)', 'ERROR: Unrecognized command-line option "', arg, '"'
    case (2)
      print '(a,a,a,/)', 'ERROR: Do not set option "', arg, '"'
    case (3)
      print '(a,/)', 'ERROR: Do not set necessary option!'
    case (4)
      print '(a,a,a,/)', 'ERROR: Unrecognized command-line parameter "', arg, '"'
    case (5)
      print '(a,a,a,/)', 'ERROR: Do not define parameter of option "',&
      arg, '"'
    case (6)
      print '(a,/)', 'ERROR: Do not set necessary parameter!'
    case (7)
      print '(a,a,a)', 'ERROR: No such file "', arg, '"'
    case (8)
      print '(a,a,a,/)', 'ERROR: No such file "', arg, '"'
      return
    case (9)
      print '(a,a,a,/)', 'ERROR: Unrecognized parameter "', arg, '" &
      in input file'
      !call print_help()
      return
    case (10)
      write(6, '(1x,a,a,a)', advance = 'no') 'WARNING: File "', arg, &
      '" is exist! Overwrite? [y/n]:'
      return
    case default
      print '(a,/)', 'ERROR!'
    end select
  else
    print '(a,/)', 'ERROR!'
  end if
  !call print_help()
  stop
end subroutine

end program ha
