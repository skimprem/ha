program ha

use hamodule

  implicit none

  character(*), parameter :: version = '1.1'
  character(1000) :: arg
  character(8) :: ncmode
  integer :: k = 0
  type(ncfile) :: nc_file

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
      nc_file%path = adjustl(arg)
    case('--ncmode')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noarg', arg, '--ncmode')
      call input_check('checkarg', arg, 'view,viewdata, viewinfo')
      ncmode = adjustl(arg)
    case default
      call input_check('noopt', arg)
    end select
  end do

  call nc_reader(nc_file, trim(ncmode))

end program ha
