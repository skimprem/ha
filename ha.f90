program ha

  use shtools
  use hamodule
  use ncmodule
  use shmodule

  implicit none

  character(*), parameter :: version = '0.1'
  character(max_name_value) :: arg
  real(8), allocatable :: cilm(:,:,:), griddh(:,:)
  real(8) :: cpu_time_1, cpu_time_2, calc_time
  integer(4) :: k = 0, i, j, &
    n, lmax, norm, sampling, csphase, lmax_calc, exitstatus, &
    un = 6
  real(8) :: memory_megabytes
  type(ncfile) :: nc_file
  type(shfile) :: sh_file
  type(haoptions) :: gridfile, ncmode, hamode, outgridfile

  gridfile%definition = .false.
  ncmode%definition = .false.
  hamode%definition = .false.
  outgridfile%definition = .false.
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
      gridfile%value = adjustl(arg)
      nc_file%path = adjustl(arg)
    case('-nm', '--ncmode')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--ncmode')
      call input_check('charg', arg, 'view')
      ncmode%definition = .true.
      ncmode%option_name = 'ncmode'
      ncmode%value = adjustl(arg)
    case('-og', '--outgrid')
      k = k + 1
      call get_command_argument(k, arg)
      call input_check('noopt', arg, '--outgrid')
      outgridfile%definition = .true.
      outgridfile%option_name = 'outgridfile'
      outgridfile%value = adjustl(arg)
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

  call nc_reader(nc_file)

  if(ncmode%definition .eqv. .true.) then
    call nc_print_info(nc_file, un)
  end if

  if(outgridfile%definition .eqv. .true.) then
    open(newunit = un, file = outgridfile%value)
    !call nc_print_data(nc_file, )
  end if


  !print *, nc_file%variable(3)%len(2)/2, nc_file%variable(3)%len(2)/2

  !read(*, *) ! pause
  !allocate( sh_file%cilm(2, nc_file%variable(3)%len(2)/2, nc_file%variable(3)%len(2)/2) )
  allocate( sh_file%griddh(nc_file%variable(3)%len(2), nc_file%variable(3)%len(1)) )

  !read(*, *) ! pause
  !deallocate(sh_file%griddh)
  !read(*, *) ! pause
  stop 'wtf?!'

  do i = 1, nc_file%variable(3)%len(2)
    do j = 1, nc_file%variable(3)%len(1)
      !sh_file%griddh(i,j) = nc_file%variable(3)%val2(j,i)%float
    end do
  end do

  if(hamode%definition .eqv. .true.) then
    sh_file%method = trim(hamode%value)
    write(un, '(a)') 'Expand..'
    select case(trim(hamode%value))
    case('dh')
      sh_file%n = nc_file%variable(3)%len(2)
      sh_file%norm = 1
      sh_file%sampling = 2
      sh_file%csphase = 1
      sh_file%lmax_calc = nc_file%variable(3)%len(2)/2-1

      call cpu_time(cpu_time_1)

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

      write(un, '(2x,a)') 'Calculation time: '//&
        number_to_string(calc_time)//&
        ' seconds'

    case('ls')
    end select

    call print_sh_info(sh_file)

  end if

end program ha
