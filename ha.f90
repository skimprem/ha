program ha

use netcdf
use hamodule

  implicit none

  character(*), parameter :: version = '1.0'
  character(*) :: arg*500 !, ncfile*250
  integer :: j = 0, i, k
  type(ncfile) :: input_file
  integer :: ncstatus

  arg = ''

  if (command_argument_count() == 0) then
    call print_help()
    stop
  end if

  do while(j < command_argument_count())
    j = j + 1
    call get_command_argument(j, arg)

    if(trim(adjustl(arg)) == '') then
      print '(a)', 'ERROR: Do not set any option!'
      call print_help('stop')
    end if

    select case(arg)
    case('--ncfile')
      j = j + 1
      call get_command_argument(j, arg)
      call input_check('noarg', arg, '--ncfile')
      call input_check('nofile', arg)
      input_file%path = adjustl(arg)
    case default
      call input_check('noopt', arg)
    end select
  end do

  input_file%cmode = nf90_nowrite

  call nc_error_check(&
    'nc_open',&
    nf90_open(&
      path = input_file%path,&
      mode = input_file%cmode,&
      ncid = input_file%ncid&
    )&
  )

  call nc_error_check(&
    'nc_inquire',&
    nf90_inquire(&
      ncid = input_file%ncid,&
      ndimensions = input_file%ndimensions,&
      nvariables = input_file%nvariables,&
      nattributes = input_file%nattributes,&
      unlimiteddimid = input_file%unlimiteddimid,&
      formatnum = input_file%formatnum&
    )&
  )

  allocate(&
    input_file%dimension(input_file%ndimensions),&
    input_file%variable(input_file%nvariables)&
  )
  
  do i = 1, input_file%ndimensions

    call nc_error_check(&
      'nc_inquire_dimension',&
      nf90_inquire_dimension(&
        ncid = input_file%ncid,&
        dimid = i,&
        name = input_file%dimension(i)%name,&
        len = input_file%dimension(i)%len&
      )&
    )

  end do

  do i = 1, input_file%nvariables

    call nc_error_check(&
      'nc_inquire_variable',&
      nf90_inquire_variable(&
        ncid = input_file%ncid,&
        varid = i,&
        name = input_file%variable(i)%name,&
        xtype = input_file%variable(i)%xtype,&
        ndims = input_file%variable(i)%ndims,&
        natts = input_file%variable(i)%natts&
      )&
    )

    allocate(&
      input_file%variable(i)%dimids(input_file%variable(i)%ndims),&
      input_file%variable(i)%attribute(input_file%variable(i)%natts)&
    )

    input_file%variable(i)%varid = i

    call nc_error_check(&
      'nc_inquire_variable',&
      nf90_inquire_variable(&
        ncid = input_file%ncid,&
        varid = input_file%variable(i)%varid,&
        dimids = input_file%variable(i)%dimids&
      )&
    )

    do k = 1, input_file%variable(i)%natts
      
      input_file%variable(i)%attribute(j)%attnum = j

      call nc_error_check(&
        'nc_inq_attname',&
        nf90_inq_attname(&
          ncid = input_file%ncid,&
          varid = input_file%variable(i)%varid,&
          attnum = input_file%variable(i)%attribute(j)%attnum,&
          name = input_file%variable(i)%attribute(j)%name&
        )&
      )
      
      call nc_error_check(&
        'nc_inquire_attribute',&
        nf90_inquire_attribute(&
          ncid = input_file%ncid,&
          varid = input_file%variable(i)%varid,&
          name = input_file%variable(i)%attribute(j)%name,&
          xtype = input_file%variable(i)%attribute(j)%xtype,&
          len = input_file%variable(i)%attribute(j)%len&
        )&
      )

    end do

  end do

  !call nc_error_check(&
    !'nc_inq_dimids',&
    !nf90_inq_dimids(&
      !ncid = input_file%ncid,&
      !ndims = input_file%ndims,&
      !dimids = input_file%dimids,&
    !)&
  !)

  !ncstatus = nf90_inq_varid(&
    !ncid = input_file%ncid,&
    !name = 'test',&
    !varid = input_file%varid&
  !)

  !call nc_error_check('nc_inq_varid', ncstatus)

  !ncstatus = nf90_inquire_variable(&
    !ncid = input_file%ncid,&
    !varid = input_file%varid,&
    !name = input_file%name,&
    !xtype = input_file%xtype,&
    !ndims = input_file%ndims,&
    !dimids = input_file%dimids,&
    !natts = input_file%natts&
  !)
  
  call print_nc_info(input_file)

!function blank_string(string)
  !implicit none
  !logical :: blank_string
  !character(*), intent(in) :: string
  !blank_string = .false. 
  !if (trim(adjustl(string)) == '') then
    !blank_string = .true. 
  !end if
!end function

!subroutine print_error(i, arg)
  !! i-parameter
  !! 1: unrecognized option -> stop
  !! 2: do not set option 'arg' -> stop
  !! 3: do not set necessary option -> stop
  !! 4: unrecognized parameter 'arg' -> stop
  !! 5: do not set parameter 'arg' -> stop
  !! 6: do not set necessary parameter -> stop
  !! 7: file not exist -> stop
  !! 8: file not exist -> return
  !! 9: unrecognized parameter 'arg' -> return
  !implicit none
  !character(*), optional :: arg
  !integer, optional :: i
  !!print '(/)'
  !if (present(i)) then
    !select case(i)
    !case (1)
      !print '(a,a,a,/)', 'ERROR: Unrecognized command-line option "', arg, '"'
    !case (2)
      !print '(a,a,a,/)', 'ERROR: Do not set option "', arg, '"'
    !case (3)
      !print '(a,/)', 'ERROR: Do not set necessary option!'
    !case (4)
      !print '(a,a,a,/)', 'ERROR: Unrecognized command-line parameter "', arg, '"'
    !case (5)
      !print '(a,a,a,/)', 'ERROR: Do not define parameter of option "',&
      !arg, '"'
    !case (6)
      !print '(a,/)', 'ERROR: Do not set necessary parameter!'
    !case (7)
      !print '(a,a,a)', 'ERROR: No such file "', arg, '"'
    !case (8)
      !print '(a,a,a,/)', 'ERROR: No such file "', arg, '"'
      !return
    !case (9)
      !print '(a,a,a,/)', 'ERROR: Unrecognized parameter "', arg, '" &
      !in input file'
      !!call print_help()
      !return
    !case (10)
      !write(6, '(1x,a,a,a)', advance = 'no') 'WARNING: File "', arg, &
      !'" is exist! Overwrite? [y/n]:'
      !return
    !case default
      !print '(a,/)', 'ERROR!'
    !end select
  !else
    !print '(a,/)', 'ERROR!'
  !end if
  !!call print_help()
  !stop
!end subroutine

end program ha
