program ha

use netcdf
use hamodule

  implicit none

  character(*), parameter :: version = '1.0'
  character(*) :: arg*500, temp*500 !, ncfile*250
  integer :: k = 0, i, j
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

      select case(input_file%variable(i)%attribute(j)%xtype)
      case(0)
        ! 0
      case(1)
        ! NC_BYTE: 8-bit signed integer
      case(7)
        ! NC_UBYTE: 8-bit unsigned integer
      case(2)
        ! NC_CHAR: 8-bit character byte
        !allocate(&
          !input_file%variable(i)%attribute(j)%value_char(&
            !input_file%variable(i)%attribute(j)%len&
          !)&
        !)
        call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = input_file%ncid,&
            varid = input_file%variable(i)%varid,&
            name = input_file%variable(i)%attribute(j)%name,&
            values = input_file%variable(i)%attribute(j)%value_char&
          )&
        )

      case(3)
        ! NC_SHORT: 16-bit signed integer
      case(8)
        ! NC_USHORT: 16-bit unsigned integer
      case(4)
        ! NC_INT: (NC_LONG): 32-bit signed integer
      case(9)
        ! NC_UINT: 32-bit unsigned integer
      case(10)
        ! NC_INT64: 64-bit signed integer
      case(11)
        ! NC_UINT64: 64-bit unsigned integer
      case(5)
        ! NC_FLOAT: 32-bit floating point
        allocate(&
          input_file%variable(i)%attribute(j)%value_real4(&
            input_file%variable(i)%attribute(j)%len&
          )&
        )
        call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = input_file%ncid,&
            varid = input_file%variable(i)%varid,&
            name = input_file%variable(i)%attribute(j)%name,&
            values = input_file%variable(i)%attribute(j)%value_real4&
          )&
        )
      case(6)
        ! NC_DOUBLE: 64-bit floating point
        allocate(&
          input_file%variable(i)%attribute(j)%value_real8(&
            input_file%variable(i)%attribute(j)%len&
          )&
        )
        call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = input_file%ncid,&
            varid = input_file%variable(i)%varid,&
            name = input_file%variable(i)%attribute(j)%name,&
            values = input_file%variable(i)%attribute(j)%value_real8&
          )&
        )
      case(12)
        ! NC_STRING: variable length character string
      end select
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

end program ha