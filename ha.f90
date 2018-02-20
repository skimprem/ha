program ha

use netcdf
use hamodule

  implicit none

  character(*), parameter :: version = '1.0'
  character(*) :: arg*500, temp*500 !, ncfile*250
  integer :: k = 0, i, j
  integer(4), dimension(:), allocatable :: dimslen
  type(ncfile) :: input_file
  integer :: ncstatus

  arg = ''

  if (command_argument_count() == 0) then
    call print_help()
    stop
  end if

  do while(k < command_argument_count())
    k = k + 1
    call get_command_argument(k, arg)

    if(trim(adjustl(arg)) == '') then
      print '(a)', 'ERROR: Do not set any option!'
      call print_help('stop')
    end if

    select case(arg)
    case('--ncfile')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noarg', arg, '--ncfile')
      call input_check('nofile', arg)
      input_file%path = adjustl(arg)
    case default
      call input_check('noopt', arg)
    end select
  end do

  call nc_reader()

end program ha
