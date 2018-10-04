program ha

  use shtools
  use hamodule
  use ncmodule
  use shmodule

  implicit none

  character(*), parameter :: version = '0.1'
  character(max_name_value) :: arg
  real(8) :: cpu_time_1, cpu_time_2, calc_time
  integer(4) :: k, i, j, stdout
  type(ncfile) :: nc_file
  type(shfile) :: sh_file
  type(haoptions) :: grid_file&
                    ,nc_mode&
                    ,ha_mode&
                    ,out_grid_file&
                    ,vb_mode&
                    ,out_coef_file

  k = 0
  stdout = 6
  grid_file%definition = .false.
  nc_mode%definition = .false.
  ha_mode%definition = .false.
  out_grid_file%definition = .false.
  vb_mode%definition = .false.
  out_coef_file%definition = .false.
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
      grid_file%definition = .true.
      grid_file%option_name =  'gridfile'
      grid_file%value = trim(adjustl(arg))
      nc_file%path = trim(adjustl(arg))
    case('-nm', '--ncmode')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--ncmode')
      nc_mode%definition = .true.
      nc_mode%option_name = 'ncmode'
      nc_mode%value = trim(adjustl(arg))
    case('-og', '--outgrid')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--outgrid')
      out_grid_file%definition = .true.
      out_grid_file%option_name = 'outgridfile'
      out_grid_file%value = trim(adjustl(arg))
    case('-oc', '--outcs')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--outgrid')
      out_coef_file%definition = .true.
      out_coef_file%option_name = 'outgridfile'
      out_coef_file%value = trim(adjustl(arg))
    case('-vm', '--verbose')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--verbose')
      vb_mode%definition = .true.
      vb_mode%option_name = 'verbosemode'
      vb_mode%value = trim(adjustl(arg))
    case('-lm', '--lmax')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--lmax')
      call input_check('noint', arg)
    case('-hm', '--hamode')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--hamode')
      ! dh - Driskoll and Healy method
      ! ls - Least squares 
      call input_check('charg', arg, 'dh, ls')
      ha_mode%definition = .true.
      ha_mode%option_name = 'hamode'
      ha_mode%value = adjustl(arg)
    case('-h', '--help')
      k = k + 1
      call get_command_argument(k, arg)
      call print_help('stop')
    case default
      call input_check('unopt', arg)
    end select
  end do

  if(vb_mode%definition .eqv. .true.) then
    call nc_reader(nc_file, vb_mode%value)
  else
    call nc_reader(nc_file)
  end if

  if(nc_mode%definition .eqv. .true.) then
    if(vb_mode%definition .eqv. .true.) then
      !call nc_print_info(nc_file, nc_mode%value, vb_mode%value)
    else
      !call nc_print_info(nc_file, nc_mode%value)
    end if
  end if

  if(out_grid_file%definition .eqv. .true.) then
    !call nc_variable_conv(nc_file%variable(3), nc_file%variable(3)%value%double_2)
    !nc_file%variable(3)%xtype = nf90_double
    if(vb_mode%definition .eqv. .true.) then
      call nc_print_data_two_dim(nc_file, out_grid_file%value, vb_mode%value)
    else
      call nc_print_data_two_dim(nc_file, out_grid_file%value)
    end if
  end if

  !if(ha_mode%definition .eqv. .true.) then

    !call nc_variable_conv(nc_file%variable(3), sh_file%griddh)

    !if(int(nc_file%variable(3)%len(1), 4) / int(nc_file%variable(3)%len(2), 4) == 2) then
      !sh_file%griddh = transpose(sh_file%griddh) 
    !end if

    !sh_file%method = trim(ha_mode%value)
    !write(stdout, '(a)') 'Expand..'
    !select case(trim(ha_mode%value))
    !case('dh')
      !sh_file%n = nc_file%variable(3)%len(2)
      !sh_file%norm = 1
      !sh_file%sampling = 2
      !sh_file%csphase = 1
      !sh_file%lmax_calc = nc_file%variable(3)%len(2)/2-1

      !call cpu_time(cpu_time_1)

      !allocate( sh_file%cilm(2, nc_file%variable(3)%len(2)/2, nc_file%variable(3)%len(2)/2) )

      !call shexpanddh(&
        !sh_file%griddh&! input, real*8, dimension (n, n) or (n, 2*n)
        !,n = sh_file%n&! input, integer
        !,cilm = sh_file%cilm&! output, real*8, dimension (2, n/2, n/2) or (2, lmax_calc+1, lmax_calc+1)
        !,lmax = sh_file%lmax&! output, integer
        !,norm = sh_file%norm&! input, optional, integer, default = 1
        !,sampling = sh_file%sampling&! input, optional, integer, default = 1
        !,csphase = sh_file%csphase&! input, optional, integer, default = 1
        !!,exitstatus = sh_file%exitstatus&! output, optional, integer
        !)

      !call cpu_time(cpu_time_2)

      !calc_time = cpu_time_2 - cpu_time_1

      !write(stdout, '(2x,a)') 'Calculation time: '//&
      !number_to_string(calc_time, frmt = '(f100.3)')//&
      !' seconds'

    !case('ls')
    !end select

    !call sh_print_info(sh_file)

    !if(out_coef_file%definition .eqv. .true.) then
      !if(vb_mode%definition .eqv. .true.) then
        !call sh_print_data(sh_file, out_coef_file%value, vb_mode%value)
      !else
        !call sh_print_data(sh_file, out_coef_file%value)
      !end if
    !end if

  !end if
end program ha
