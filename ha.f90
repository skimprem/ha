program ha

  use shtools
  use hamodule
  use ncmodule

  implicit none
  
  character(*), parameter :: version = '1.1'
  character(max_name_value) :: arg
  real(8), allocatable :: cilm(:,:,:), griddh(:,:)
  integer :: k = 0, i, j, &
    n, lmax, norm, sampling, csphase, lmax_calc, exitstatus, &
    un = 6
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

  if(hamode%definition .eqv. .true.) then
    write(un, '(a)', advance = 'no') 'Expand method: '
    select case(trim(hamode%value))
    case('dh')
      write(un, '(a)') 'Driscoll and Healy sampling theorem'
      n = nc_file%variable(3)%len(2)
      write(un, '(2x, a)') 'n: '//&
        number_to_string(iv = int(n, 4), len = num_len(iv = int(n, 4)))
      norm = 1
      write(un, '(2x, a)') 'norm: '//&
        number_to_string(iv = int(norm, 4), len = num_len(iv = int(norm, 4)))
      sampling = 2
      write(un, '(2x, a)') 'sampling: '//&
        number_to_string(iv = int(sampling, 4), len = num_len(iv = int(sampling, 4)))
      csphase = 1
      write(un, '(2x, a)') 'csphase: '//&
        number_to_string(iv = int(csphase, 4), len = num_len(iv = int(csphase, 4)))
      lmax_calc = nc_file%variable(3)%len(2)/2-1
      write(un, '(2x, a)') 'lmax_calc: '//&
        number_to_string(iv = int(lmax_calc, 4), len = num_len(iv = int(lmax_calc, 4)))

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

      write(un, '(2x, a)') 'exitstatus: '//&
        number_to_string(iv = int(exitstatus, 4), len = num_len(iv = int(exitstatus, 4)))

      do i = 1, lmax
        do j = 1, i
          write(un, *) i, j, cilm(1, i, j), cilm(2, i, j)
        end do
      end do
      !print *, cilm

    case('ls')
      print '(a)', 'Using expand method: least squares inversion'
    end select
  end if

end program ha
