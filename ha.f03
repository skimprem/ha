
use netcdf

  implicit none

  character(*), parameter :: version = '1.0'
  character(*) :: arg*250, ncfile*250
  integer(4) :: j

  do while(j < command_argument_count())
      j = j + 1
      call get_command_argument(j, arg)

      select case(arg)
      case('-nf', '--ncfile')
        j = j + 1
        call get_command_argument(j, arg)
        !if(blank_string(arg)) call print_error(5, '-nf')
        call input_check( blank_string(arg), arg )
        ncfile = adjustl(arg)
        print *, 'input ncfile = ', trim(adjustl(ncfile))
      end select

  end do

end

subroutine input_check(status, arg)
  implicit none
  integer, intent (in) :: status
  character(*), intent(in), optional :: arg

  if(status /= 0) then
    print *, trim(input_error(status, arg))
    stop 'Stopped'
  end if
end subroutine input_check

function input_error(status, arg)
  implicit none

  character(500), intent(out) :: input_error
  character(*), intent(in), optional :: arg
  integer, intent(in) :: status

  select case(status)
    case(1)
      input_error = 'ERROR: Do not set parameter of option "', arg, "'"
  end select
end function input_error

function blank_string(string)
  implicit none
  integer :: blank_string
  character(*) :: string
  blank_string = 0
  if (trim(adjustl(string)) == '') then
    blank_string = 1 
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
  print '(/)'
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
      print '(a,a,a,/)', 'ERROR: Do not set parameter of option "',&
      arg, '"'
    case (6)
      print '(a,/)', 'ERROR: Do not set necessary parameter!'
    case (7)
      print '(a,a,a,/)', 'ERROR: No such file "', arg, '"'
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

