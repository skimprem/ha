program ha

  use shtools
  use hamodule
  use ncmodule

  implicit none
  
  character(*), parameter :: version = '1.1'
  character(max_name_value) :: arg
  real(8), allocatable :: cilm(:,:,:), griddh(:,:)
  integer :: k = 0, i, j, n, lmax, norm, sampling, csphase, lmax_calc, exitstatus
  type(ncfile) :: nc_file
  type(haoptions) :: gridfile, ncmode, hamode

  gridfile%definition = .false.
  ncmode%definition = .false.
  hamode%definition = .false.
  arg = ''

  if (command_argument_COUNT() == 0) then
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
      call input_check('charg', arg, 'view, viewdata, viewinfo')
      ncmode%definition = .true.
      ncmode%option_name = 'ncmode'
      ncmode%value = adjustl(arg)
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

  call nc_reader(nc_file, trim(ncmode%value))

  allocate( cilm(2, nc_file%variable(3)%len(2)/2, nc_file%variable(3)%len(2)/2), &
       griddh(nc_file%variable(3)%len(2), nc_file%variable(3)%len(1)) )

  do i = 1, nc_file%variable(3)%len(2)
    do j = 1, nc_file%variable(3)%len(1)
      griddh(i,j) = nc_file%variable(3)%val2(j,i)%float
    end do
  end do

  select case(trim(hamode%value))
  case('dh')
    print '(a)', 'Using expand method: Driscoll and Healy sampling theorem'
    n = nc_file%variable(3)%len(2)
    print *, 'n = ', n
    norm = 1
    print *, 'norm = ', norm
    sampling = 2
    print *, 'sampling = ', sampling
    csphase = 1
    print *, 'csphase = ', csphase
    lmax_calc = nc_file%variable(3)%len(2)/2-1
    print *, 'lmax_calc = ', lmax_calc

    call shexpanddh(&
      griddh,&! input, real*8, dimension (n, n) or (n, 2*n)
      n = n,&! input, integer
      cilm = cilm,&! output, real*8, dimension (2, n/2, n/2) or (2, lmax_calc+1, lmax_calc+1)
      lmax = lmax,&! output, integer
      norm = norm,&! input, optional, integer, default = 1
      sampling = sampling, &! input, optional, integer, default = 1
      csphase = csphase,&! input, optional, integer, default = 1
      exitstatus = exitstatus&! output, optional, integer
      )

    !print *, exitstatus

    do i = 1, lmax
      do j = 1, i
        print *, i, j, cilm(1, i, j), cilm(2, i, j)
      end do
    end do
    !print *, cilm

  case('ls')
    print '(a)', 'Using expand method: least squares inversion'
  end select

end program ha
