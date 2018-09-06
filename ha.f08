program ha

  use shtools
  use hamodule
  use ncmodule
  use shmodule

  implicit none

  character(*), parameter :: version = '0.1'
  character(max_name_value) :: arg
  real(8) :: cpu_time_1, cpu_time_2, calc_time
  integer(4) :: k = 0, i, j, stdout = 6
  type(ncfile) :: nc_file
  type(shfile) :: sh_file
  type(haoptions) :: gridfile, ncmode, hamode, outgridfile, outcoeffile, verbosemode

  gridfile%definition = .false.
  ncmode%definition = .false.
  hamode%definition = .false.
  outgridfile%definition = .false.
  verbosemode%definition = .false.
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
      gridfile%definition = .true.
      gridfile%option_name =  'gridfile'
      gridfile%value = trim(adjustl(arg))
      nc_file%path = trim(adjustl(arg))
    case('-nm', '--ncmode')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--ncmode')
      ncmode%definition = .true.
      ncmode%option_name = 'ncmode'
      ncmode%value = trim(adjustl(arg))
    case('-og', '--outgrid')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--outgrid')
      outgridfile%definition = .true.
      outgridfile%option_name = 'outgridfile'
      outgridfile%value = trim(adjustl(arg))
    case('-vm', '--verbose')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--verbose')
      verbosemode%definition = .true.
      verbosemode%option_name = 'verbosemode'
      verbosemode%value = trim(adjustl(arg))
    case('-oc', '--outcoef')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--outcoef')
      outcoeffile%definition = .true.
      outcoeffile%option_name = 'outcoeffile'
      outcoeffile%value = trim(adjustl(arg))
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
      hamode%definition = .true.
      hamode%option_name = 'hamode'
      hamode%value = adjustl(arg)
    case('-h', '--help')
      k = k + 1
      call get_command_argument(k, arg)
      call print_help('stop')
    case default
      call input_check('unopt', arg)
    end select
  end do

  if(verbosemode%definition .eqv. .true.) then
    call nc_reader(nc_file, verbosemode%value)
  else
    call nc_reader(nc_file)
  end if

  if(ncmode%definition .eqv. .true.) then
    if(verbosemode%definition .eqv. .true.) then
      call nc_print_info(nc_file, ncmode%value, verbosemode%value)
    else
      call nc_print_info(nc_file, ncmode%value)
    end if
  end if

  if(outgridfile%definition .eqv. .true.) then
    call nc_variable_conv(nc_file%variable(3), nc_file%variable(3)%value%double_2)
    nc_file%variable(3)%xtype = nf90_double
    if(verbosemode%definition .eqv. .true.) then
      call nc_print_data(nc_file, outgridfile%value, verbosemode%value)
    else
      call nc_print_data(nc_file, outgridfile%value)
    end if
  end if

  if(hamode%definition .eqv. .true.) then

    call nc_variable_conv(nc_file%variable(3), sh_file%griddh)

    if(int(nc_file%variable(3)%len(1), 4) / int(nc_file%variable(3)%len(2), 4) == 2) then
      sh_file%griddh = transpose(sh_file%griddh) 
    end if

    sh_file%method = trim(hamode%value)
    write(stdout, '(a)') 'Expand..'
    select case(trim(hamode%value))
    case('dh')
      sh_file%n = nc_file%variable(3)%len(2)
      sh_file%norm = 1
      sh_file%sampling = 2
      sh_file%csphase = 1
      sh_file%lmax_calc = nc_file%variable(3)%len(2)/2-1

      call cpu_time(cpu_time_1)

      allocate( sh_file%cilm(2, nc_file%variable(3)%len(2)/2, nc_file%variable(3)%len(2)/2) )

      call shexpanddh(&
        sh_file%griddh,&! input, real*8, dimension (n, n) or (n, 2*n)
        n = sh_file%n,&! input, integer
        cilm = sh_file%cilm,&! output, real*8, dimension (2, n/2, n/2) or (2, lmax_calc+1, lmax_calc+1)
        lmax = sh_file%lmax,&! output, integer
        norm = sh_file%norm,&! input, optional, integer, default = 1
        sampling = sh_file%sampling, &! input, optional, integer, default = 1
        csphase = sh_file%csphase,&! input, optional, integer, default = 1
        exitstatus = sh_file%exitstatus&! output, optional, integer
        )

      call cpu_time(cpu_time_2)

      calc_time = cpu_time_2 - cpu_time_1

      write(stdout, '(2x,a)') 'Calculation time: '//&
        number_to_string(calc_time, frmt = '(f100.3)')//&
        ' seconds'

    case('ls')
    end select

    call sh_print_info(sh_file)

  end if

end program ha
