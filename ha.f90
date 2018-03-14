program ha

use SHTOOLS
use hamodule
use ncmodule

  implicit none

  character(*), parameter :: version = '1.1'
  character(1000) :: arg
  character(8) :: ncmode
  integer :: k = 0
  type(ncfile) :: nc_file

  arg = ''

  if (command_argument_count() == 0) then
    call input_check('noarg', arg)
    !print '(a)', 'ERROR: Do not set any option!'
    !call print_help('stop')
  end if

  do while(k < command_argument_count())
    k = k + 1
    call get_command_argument(k, arg)

    select case(arg)
    case('-nf', '--ncfile')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--ncfile')
      call input_check('nofile', arg)
      nc_file%path = adjustl(arg)
    case('nm', '--ncmode')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--ncmode')
      call input_check('charg', arg, 'view, viewdata, viewinfo')
      ncmode = adjustl(arg)
    case('hm', '--hamode')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noarg', arg, '--hamode')
      call input_check('charg', arg, 'ni, ai, ls, st')
    case('-h', '--help')
      k = k + 1
      call get_command_argument(k, arg)
      call print_help('stop')
    case default
      call input_check('unopt', arg)
    end select
  end do

  call nc_reader(nc_file, trim(ncmode))

  call shexpanddh(&
    griddh = ,&! input, real*8, dimension (n, n) or (n, 2*n)
    n = ,&! input, integer
    cilm = ,&! output, real*8, dimension (2, n/2, n/2) or (2, lmax_calc+1, lmax_calc+1)
    lmax = ,&! output, integer
    norm = ,&! input, optional, integer, default = 1
    sampling = ,&! input, optional, integer, default = 1
    csphase = ,&! input, optional, integer, default = 1
    lmax_calc = ,&! input, optional, integer, default = lmax
    exitstatus = &! output, optional, integer
  )

end program ha
