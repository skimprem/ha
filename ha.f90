PROGRAM ha

  USE SHTOOLS
  USE hamodule
  USE ncmodule

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: version = '1.1'
  CHARACTER(1000) :: arg
  CHARACTER(8) :: ncmode
  REAL(8), ALLOCATABLE :: cilm(:,:,:), griddh(:,:)
  INTEGER :: k = 0, i, j, n, lmax, norm, sampling, csphase, lmax_calc, exitstatus
  TYPE(ncfile) :: nc_file

  arg = ''

  IF (command_argument_COUNT() == 0) THEN
     CALL input_check('noarg', arg)
     !print '(a)', 'ERROR: Do not set any option!'
     !call print_help('stop')
  END IF

  DO WHILE(k < command_argument_COUNT())
     k = k + 1
     CALL get_command_ARGUMENT(k, arg)

     SELECT CASE(arg)
     CASE('-nf', '--ncfile')
        k = k + 1
        CALL get_command_ARGUMENT(k, arg)
        CALL input_check('noopt', arg, '--ncfile')
        CALL input_check('nofile', arg)
        nc_file%path = ADJUSTL(arg)
     CASE('-nm', '--ncmode')
        k = k + 1
        CALL get_command_ARGUMENT(k, arg)
        CALL input_check('noopt', arg, '--ncmode')
        CALL input_check('charg', arg, 'view, viewdata, viewinfo')
        ncmode = ADJUSTL(arg)
     CASE('-lm', '--lmax')
        k = k + 1
        CALL get_command_ARGUMENT(k, arg)
        CALL input_check('noopt', arg, '--lmax')
        CALL input_check('noint', arg)
     CASE('-hm', '--hamode')
        k = k + 1
        CALL get_command_ARGUMENT(k, arg)
        CALL input_check('noopt', arg, '--hamode')
        CALL input_check('charg', arg, 'ni, ai, ls, st')
     CASE('-h', '--help')
        k = k + 1
        CALL get_command_ARGUMENT(k, arg)
        CALL print_help('stop')
     CASE default
        CALL input_check('unopt', arg)
     END SELECT
  END DO

  CALL nc_reader(nc_file, TRIM(ncmode))

  ALLOCATE( cilm(2, nc_file%variable(3)%LEN(2)/2, nc_file%variable(3)%LEN(2)/2), &
       griddh(nc_file%variable(3)%LEN(2), nc_file%variable(3)%LEN(1)) )

  DO i = 1, nc_file%variable(3)%LEN(2)
     DO j = 1, nc_file%variable(3)%LEN(1)
        griddh(i,j) = nc_file%variable(3)%val2(j,i)%float
     END DO
  END DO

  n = nc_file%variable(3)%LEN(2)
  !print *, 'n = ', n
  norm = 1
  !print *, 'norm = ', norm
  sampling = 2
  !print *, 'sampling = ', sampling
  csphase = 1
  !print *, 'csphase = ', csphase
  !lmax_calc = nc_file%variable(3)%len(2)/2-1
  !print *, 'lmax_calc = ', lmax_calc


  CALL shexpanddh(&
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

  DO i = 1, lmax
     DO j = 1, i
        PRINT *, i, j, cilm(1, i, j), cilm(2, i, j)
     END DO
  END DO
  !print *, cilm
END PROGRAM ha
