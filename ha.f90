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
    input_file%variable(input_file%nvariables),&
    input_file%attribute(input_file%nattributes)&
  )

  do i = 1, input_file%nattributes
    
    input_file%attribute(i)%attnum = i

    call nc_error_check(&
      'nc_inq_attname',&
      nf90_inq_attname(&
        ncid = input_file%ncid,&
        varid = nf90_global,&
        attnum = input_file%attribute(i)%attnum,&
        name = input_file%attribute(i)%name&
      )&
    )

    call nc_error_check(&
      'nc_inquire_attribute',&
      nf90_inquire_attribute(&
        ncid = input_file%ncid,&
        varid = nf90_global,&
        name = input_file%attribute(i)%name,&
        xtype = input_file%attribute(i)%xtype,&
        len = input_file%attribute(i)%len&
      )&
    )

    call get_att_xtype(&
      ncid = input_file%ncid,&
      varid = nf90_global,&
      xtype = input_file%attribute(i)%xtype,&
      name = input_file%attribute(i)%name,&
      len = input_file%attribute(i)%len,&
      value = input_file%attribute(i)%value&
    )

  end do
  
  do i = 1, input_file%ndimensions

    input_file%dimension(i)%dimid = i

    call nc_error_check(&
      'nc_inquire_dimension',&
      nf90_inquire_dimension(&
        ncid = input_file%ncid,&
        dimid = input_file%dimension(i)%dimid,&
        name = input_file%dimension(i)%name,&
        len = input_file%dimension(i)%len&
      )&
    )

  end do

  do i = 1, input_file%nvariables
    
    input_file%variable(i)%varid = i

    call nc_error_check(&
      'nc_inquire_variable',&
      nf90_inquire_variable(&
        ncid = input_file%ncid,&
        varid = input_file%variable(i)%varid,&
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

    call nc_error_check(&
      'nc_inquire_variable',&
      nf90_inquire_variable(&
        ncid = input_file%ncid,&
        varid = input_file%variable(i)%varid,&
        dimids = input_file%variable(i)%dimids&
      )&
    )

    do j = 1, input_file%variable(i)%natts
      
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

      call get_att_xtype(&
        input_file%ncid,&
        input_file%variable(i)%varid,&
        input_file%variable(i)%attribute(j)%xtype,&
        input_file%variable(i)%attribute(j)%name,&
        input_file%variable(i)%attribute(j)%len,&
        input_file%variable(i)%attribute(j)%value&
      )

    end do

    allocate( input_file%variable(i)%len(input_file%variable(i)%ndims) )
    
    do j = 1, input_file%variable(i)%ndims
      call nc_error_check(&
        'nc_inquire_dimension',&
        nf90_inquire_dimension(&
          ncid = input_file%ncid,&
          dimid = input_file%variable(i)%dimids(j),&
          len = input_file%variable(i)%len(j)&
        )&
      )
    end do


    select case(input_file%variable(i)%ndims)
    case(1)
      call get_var_xtype(&
        ncid = input_file%ncid,&
        varid = input_file%variable(i)%varid,&
        xtype = input_file%variable(i)%xtype,&
        ndims = input_file%variable(i)%ndims,&
        len = input_file%variable(i)%len,&
        val1 = input_file%variable(i)%val1&
      )
    case(2)
      call get_var_xtype(&
        ncid = input_file%ncid,&
        varid = input_file%variable(i)%varid,&
        xtype = input_file%variable(i)%xtype,&
        ndims = input_file%variable(i)%ndims,&
        len = input_file%variable(i)%len,&
        val2 = input_file%variable(i)%val2&
      )
    end select

  end do
 
  call print_nc_info(input_file)

end program ha
