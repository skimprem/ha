module ncmodule

  use netcdf

  type ncdimensions
    character(nf90_max_name) ::&
        name
    integer ::&
        dimid,&
        len
  end type ncdimensions

  type ncvalues
    integer(2) :: short
    integer(4) :: int
    integer(8) :: int64
    real(4) :: float
    real(8) :: double
    character(nf90_max_name) :: char
    character(nf90_max_name) :: string
  end type ncvalues

  type ncattributes
    character(nf90_max_name) ::&
        name
    integer ::&
        xtype,&
        len,&
        attnum 
    type(ncvalues), dimension(:), allocatable ::&
        value
  end type ncattributes

  type ncxtypes
    character(nf90_max_name) ::&
        name
    integer ::&
        type,&
        size
  end type ncxtypes

  type ncvariables
    character(nf90_max_name) ::&
        name
    integer ::&
        varid,&
        xtype,&
        ndims,&
        natts
    integer, dimension(:), allocatable ::&
        dimids
    integer, dimension(:), allocatable ::&
        len
    type(ncattributes), dimension(:), allocatable :: attribute
    type(ncvalues), dimension(:), allocatable :: val1
    type(ncvalues), dimension(:,:), allocatable :: val2
    type(ncvalues), dimension(:,:,:), allocatable :: val3
  end type ncvariables

  type ncfile
    character(1000) :: path
    character(nf90_max_name) ::& !
        name,&
        title,&
        history,&
        conventions
    integer ::& !
        cmode,& !
        ncid,& !
        ndimensions,& !
        nvariables,& !
        nattributes,& !
        unlimiteddimid,& !
        formatnum !
    type(ncdimensions), dimension(:), allocatable :: dimension
    type(ncvariables), dimension(:), allocatable :: variable
    type(ncattributes), dimension(:), allocatable :: attribute
  end type ncfile

contains

  subroutine get_att_xtype(&
       ncid,& !
       varid,& !
       xtype,& !
       name,& !
       len,& !
       value) !

    use netcdf

    implicit none

    integer, intent(in) :: ncid, varid, xtype, len
    character(*), intent(in) :: name
    type(ncvalues), intent(out), dimension(:), allocatable :: value

    allocate( value(len) )

    select case(xtype)
    case(0)
      ! 0
    case(nf90_byte)
      ! NC_BYTE: 8-bit signed integer
    case(nf90_ubyte)
      ! NC_UBYTE: 8-bit unsigned integer
    case(nf90_char)
      ! NC_CHAR: 8-bit character byte
      deallocate( value )
      allocate( value(1) )
      call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = value(1)%char&
            )&
          )
    case(nf90_short)
      ! NC_SHORT: 16-bit signed integer
      call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = value%short&
            )&
          )
    case(nf90_ushort)
      ! NC_USHORT: 16-bit unsigned integer
    case(nf90_int)
      ! NC_int: (NC_LONG): 32-bit signed integer
      call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = value%int&
            )&
          )
    case(nf90_uint)
       ! NC_Uint: 32-bit unsigned integer
    case(nf90_int64)
       ! NC_int64: 64-bit signed integer
       call nc_error_check(&
            'nc_get_att',&
            nf90_get_att(&
              ncid = ncid,&
              varid = varid,&
              name = name,&
              values = value%int64&
              )&
            )
    case(nf90_uint64)
       ! NC_Uint64: 64-bit unsigned integer
    case(nf90_float)
       ! NC_FLOAT: 32-bit floating point
       call nc_error_check(&
            'nc_get_att',&
            nf90_get_att(&
              ncid = ncid,&
              varid = varid,&
              name = name,&
              values = value%float&
              )&
            )
    case(nf90_double)
       ! NC_double: 64-bit floating point
       call nc_error_check(&
            'nc_get_att',&
            nf90_get_att(&
              ncid = ncid,&
              varid = varid,&
              name = name,&
              values = value%double&
              )&
            )
    case(nf90_string)
       ! NC_STRING: variable length character string
    end select

  end subroutine get_att_xtype

  subroutine get_var_xtype(&
       ncid,& !
       varid,& !
       xtype,& !
       ndims,& !
       len,& !
       val1,& !
       val2,& !
       val3) !

    implicit none
    integer, intent(in) :: ncid, varid, xtype, ndims
    integer, intent(in), dimension(ndims) :: len
    integer :: allocate_status, un = 6, i, j, k
    type(ncvalues), intent(out), dimension(:), allocatable, optional :: val1
    type(ncvalues), intent(out), dimension(:,:), allocatable, optional :: val2
    type(ncvalues), intent(out), dimension(:,:,:), allocatable, optional :: val3

    allocate_status = nc_allocate(ndims, len, val1, val2, val3)
    
    if(allocate_status /= 0) then
      select case(allocate_status)
      case(5014)
        stop 'You do not have free memory?'
      end select
    end if

    select case(xtype)
    case(nf90_byte)
      write(un, '(a)') trim(nc_xtype_name(xtype))//' not set'
      stop
    case(nf90_ubyte)
      write(un, '(a)') trim(nc_xtype_name(xtype))//' not set'
      stop 
    case(nf90_char)
      write(un, '(a)') trim(nc_xtype_name(xtype))//' not set'
      stop 
    case(nf90_short)
      select case(ndims)
      case(1)
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = val1%short&
               )&
             )
      case(2)
         call nc_error_check(&
              'nc_get_var',&
              nf90_get_var(&
                ncid = ncid,&
                varid = varid,&
                values = val2%short&
                )&
               )
      case(3)
         call nc_error_check(&
              'nc_get_var',&
              nf90_get_var(&
                ncid = ncid,&
                varid = varid,&
                values = val3%short&
                )&
              )
      end select
    case(nf90_ushort)
      write(un, '(a)') trim(nc_xtype_name(xtype))//' not set'
      stop 
    case(nf90_int)
      write(un, '(a)') trim(nc_xtype_name(xtype))//' not set'
      stop 
    case(nf90_uint)
      write(un, '(a)') trim(nc_xtype_name(xtype))//' not set'
      stop 
    case(nf90_int64)
      write(un, '(a)') trim(nc_xtype_name(xtype))//' not set'
      stop 
    case(nf90_uint64)
      write(un, '(a)') trim(nc_xtype_name(xtype))//' not set'
      stop 
    case(nf90_float)
       select case(ndims)
       case(1)
          call nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
                 ncid = ncid,&
                 varid = varid,&
                 values = val1%float&
                 )&
               )
       case(2)
          call nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
                 ncid = ncid,&
                 varid = varid,&
                 values = val2%float&
                 )&
               )
       case(3)
          call nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
                 ncid = ncid,&
                 varid = varid,&
                 values = val3%float&
                 )&
               )

       end select
    case(nf90_double)
       select case(ndims)
       case(1)
          call nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
                 ncid = ncid,&
                 varid = varid,&
                 values = val1%double&
                 )&
               )
       case(2)
          call nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
                 ncid = ncid,&
                 varid = varid,&
                 values = val2%double&
                 )&
               )
       case(3)
          call nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
                 ncid = ncid,&
                 varid = varid,&
                 values = val3%double&
                 )&
               )
       end select
    case(nf90_string)
      write(un, '(a)') trim(nc_xtype_name(xtype))//' not set'
      stop 
    end select

  end subroutine get_var_xtype

  function nc_xtype_name(xtype)

    implicit none

    character(nf90_max_name) :: nc_xtype_name
    integer, intent(in) :: xtype

    select case(xtype)
    case(nf90_byte)
      nc_xtype_name = 'byte: 8-bit signed integer'
    case(nf90_ubyte)
      nc_xtype_name = 'ubyte: 8-bit unsigned integer' 
    case(nf90_char)
      nc_xtype_name = 'char: 8-bit character byte'
    case(nf90_short)
      nc_xtype_name = 'short: 16-bit signed integer'
    case(nf90_ushort)
      nc_xtype_name = 'ushort: 16-bit unsigned integer'
    case(nf90_int)
      nc_xtype_name = 'int: 32-bit signed integer'
    case(nf90_uint)
      nc_xtype_name = 'uint: 32-bit unsigned integer'
    case(nf90_int64)
      nc_xtype_name = 'int64: 64-bit signed integer'
    case(nf90_uint64)
      nc_xtype_name = 'uint64: 64-bit signed integer'
    case(nf90_float)
      nc_xtype_name = 'float: 32-bit floating point'
    case(nf90_double)
      nc_xtype_name = 'double: 64-bit floating point'
    case(nf90_string)
      nc_xtype_name = 'string: variable length character string'
    end select
    return
  end function nc_xtype_name

  subroutine nc_error_check(check_type, ncstatus)

    use netcdf
    use hamodule

    implicit none

    character(*), intent(in) :: check_type
    integer, intent(in) :: ncstatus
    integer :: un = 6

    if(ncstatus /= nf90_noerr) then
       select case(check_type)
       case('nc_open')
          write(un, '(a)')&
               'Error in nc_open: '//&
               number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
       case('nc_inquire')
          write(un, '(a)')&
               'Error in nc_inquire: '//&
               number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
       case('nc_inq_dimid')
          write(un, '(a)')&
               'Error in nc_inq_dimid: '//&
               number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
       case('nc_inquire_dimension')
          write(un, '(a)')&
               'Error in nc_inquire_dimension: '//&
               number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
       case('nc_inq_varid')
          write(un, '(a)')&
               'Error in nc_inq_varid: '//&
               number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
       case('nc_variable')
          write(un, '(a)')&
               'Error in nc_variable: '//&
               number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
       case('nc_inq_attname')
          write(un, '(a)')&
               'Error in nc_inq_attname: '//&
               number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
       case('nc_inquire_attribute')
          write(un, '(a)')&
               'Error in nc_inquire_attribute: '//&
               number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
       case('nc_get_att')
          write(un, '(a)')&
               'Error in nc_get_att: '//&
               number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))
       case('nc_')
          write(un, '(a)')&
               'Error in nc_: '//&
               number_to_string(iv = int(ncstatus, 4), len = num_len(iv = int(ncstatus, 4)))

       end select
       write(un, '(a)') trim(nf90_strerror(ncstatus))
       STOP 'Stopped!'
    end if

    return

  end subroutine nc_error_check

  subroutine print_nc_info(nc_file, type_info)

    use hamodule

    implicit none
    type(ncfile), intent(in) :: nc_file
    character(*), intent(in), optional :: type_info
    integer :: i, j, k, l, m, un = 6

    ! type_info:
    !   view
    !   viewdata
    !   viewinfo

    if(type_info == 'viewinfo' .OR. type_info == 'view') then
       write(un, '(a)') 'NetCDF version: '//trim(nf90_inq_libvers())
       write(un, '(a)') 'nc file info:'
       write(un, '(2x,a)') 'ncid: '//&
            number_to_string(iv = int(nc_file%ncid, 4),&
            len = num_len(iv = int(nc_file%ncid, 4)))
       write(un, '(2x,a)') 'path: '//trim(adjustl(nc_file%path))
       write(un, '(2x,a)') 'mode: '//&
            number_to_string(iv = int(nc_file%cmode, 4),&
            len = num_len(int(nc_file%cmode, 4)))
       write(un, '(2x,a)') 'ndimensions: '//&
            number_to_string(iv = int(nc_file%ndimensions, 4),&
            len = num_len(int(nc_file%ndimensions, 4)))
       do i = 1, nc_file%ndimensions
          write(un, '(4x,a)') 'name: '//trim(nc_file%dimension(i)%name)
          write(un, '(6x,a)') 'len: '//&
               number_to_string(iv = int(nc_file%dimension(i)%len, 4),&
               len = num_len(iv = int(nc_file%dimension(i)%len,4)))
       end do
       write(un, '(2x,a)') 'nVariables: '//&
            number_to_string(iv = int(nc_file%nvariables, 4),&
            len = num_len(iv = int(nc_file%nvariables, 4)))
       do i = 1, nc_file%nvariables
          write(un, '(4x,a)') 'name: '//trim(nc_file%variable(i)%name)
          write(un, '(6x,a)') 'xtype: '//&
               number_to_string(iv = int(nc_file%variable(i)%xtype, 4),&
               len = num_len(iv = int(nc_file%variable(i)%xtype, 4)))
          write(un, '(6x,a)') 'ndims: '//&
               number_to_string(iv = int(nc_file%variable(i)%ndims, 4),&
               len = num_len(iv = int(nc_file%variable(i)%ndims, 4)))
          do j = 1, nc_file%variable(i)%ndims
             write(un, '(8x,a)') 'dimid: '//&
                  number_to_string(iv = int(nc_file%variable(i)%dimids(j), 4),&
                  len = num_len(iv = int(nc_file%variable(i)%dimids(j), 4)))
          end do
          write(un, '(6x,a)') 'natts: '//&
               number_to_string(iv = int(nc_file%variable(i)%natts, 4),&
               len = num_len(iv = int(nc_file%variable(i)%natts, 4)))
          do j = 1, nc_file%variable(i)%natts
             write(un, '(8x,a)') 'name: '//&
                  trim(nc_file%variable(i)%attribute(j)%name)
             write(un, '(10x,a)') 'xtype: '//&
                  number_to_string(iv = int(nc_file%variable(i)%attribute(j)%xtype, 4),&
                  len = num_len(iv = int(nc_file%variable(i)%attribute(j)%xtype, 4)))
             write(un, '(10x,a)') 'len: '//&
                  number_to_string(iv = int(nc_file%variable(i)%attribute(j)%len, 4),&
                  len = num_len(iv = int(nc_file%variable(i)%attribute(j)%len, 4)))

             select case(nc_file%variable(i)%attribute(j)%xtype)
             case(nf90_float)
                do k = 1, nc_file%variable(i)%attribute(j)%len
                   write(un, '(10x,a)')&
                        'value '//&
                        number_to_string(iv = int(k, 4), len = num_len(iv = int(k, 4)))//': '//&
                        number_to_string(rv = real(nc_file%variable(i)%attribute(j)%value(k)%float, 4),&
                        len = num_len(rv = real(nc_file%variable(i)%attribute(j)%value(k)%float, 4)))!,&
                   !frmt = '(f10.3)')
                end do
             case(nf90_double)
                do k = 1, nc_file%variable(i)%attribute(j)%len
                   write(un, '(10x,a)')&
                        'value '//&
                        number_to_string(iv = int(k, 4), len = num_len(iv = int(k, 4)))//': '//&
                        number_to_string(rv = real(nc_file%variable(i)%attribute(j)%value(k)%double, 4),&
                        len = num_len(rv = real(nc_file%variable(i)%attribute(j)%value(k)%double, 4)))!,&
                   !frmt = '(f100.3)')
                end do
             case(nf90_char)
                write(un, '(10x,a)') 'value: '//&
                     trim(nc_file%variable(i)%attribute(j)%value(1)%char)
             end select
          end do
       end do
       write(un, '(2x,a)') 'nAttributes: '//&
            number_to_string(iv = int(nc_file%nattributes, 4),&
            len = num_len(iv = int(nc_file%nattributes, 4)))
       do i = 1, nc_file%nattributes
          write(un, '(4x,a)') trim(nc_file%attribute(i)%name)//': '//&
               trim(nc_file%attribute(i)%value(1)%char)
       end do
       write(un, '(2x,a)') 'unlimitedDimid: '//&
            number_to_string(iv = int(nc_file%unlimiteddimid, 4),&
            len = num_len(iv = int(nc_file%unlimiteddimid, 4)))
       write(un, '(2x,a)') 'formatNum: '//&
            number_to_string(iv = int(nc_file%formatnum, 4),&
            len = num_len(iv = int(nc_file%formatnum, 4)))
    end if

    if(type_info == 'viewdata' .or. type_info == 'view') then
      write(un, '(a)') 'viewdata'
      k = 1 
      do m = 1, nc_file%variable(k)%ndims
        do i = 1, nc_file%variable(k)%len(m)
          do l = 1, nc_file%variable(k+1)%ndims
            do j = 1, nc_file%variable(k+1)%len(l)
              !print *, &
              !number_to_string(iv = i, len = num_len(iv = i)), &
              !' = ', &
              !number_to_string(rv = real(nc_file%variable(k)%val1(i)%double, 4), &
              !len = num_len(rv = real(nc_file%variable(k)%val1(i)%double, 4))), &
              !' ; ', &
              !number_to_string(iv = j, len = num_len(iv = j)), &
              !' = ', &
              !number_to_string(rv = real(nc_file%variable(k+1)%val1(j)%double, 4), &
              !len = num_len(rv = real(nc_file%variable(k+1)%val1(j)%double, 4))), &
              !' ; ', &
              !'(', &
              !number_to_string(iv = i, len = num_len(iv = i)), &
              !',', &
              !number_to_string(iv = j, len = num_len(iv = j)), &
              !')', &
              !' = ', &
              !number_to_string(rv = real(nc_file%variable(k+2)%val2(i,j)%float, 4), &
              !len = num_len(rv = real(nc_file%variable(k+2)%val2(i,j)%float, 4)))
              write(un, '(a)') &
                  number_to_string(rv = real(nc_file%variable(k)%val1(i)%double, 4),&
                  len = num_len(rv = real(nc_file%variable(k)%val1(i)%double, 4),&
                  frmt = '(f20.2)'),&
                  frmt = '(f20.2)')//' '//&
                  number_to_string(rv = real(nc_file%variable(k+1)%val1(j)%double, 4),&
                  len = num_len(rv = real(nc_file%variable(k+1)%val1(j)%double, 4),&
                  frmt = '(f20.2)'),&
                  frmt = '(f20.2)')//' '//&
                  number_to_string(rv = real(nc_file%variable(k+2)%val2(i,j)%float, 4),&
                  len = num_len(rv = real(nc_file%variable(k+2)%val2(i,j)%float, 4),&
                  frmt = '(f20.4)'),&
                  frmt = '(f20.4)')
            end do
          end do
        end do
      end do
    end if

    return

  end subroutine print_nc_info

  subroutine nc_reader(input_file, mode)

    use netcdf

    implicit none
    type(ncfile), intent(inout) :: input_file
    character(*), intent(in), optional :: mode
    integer :: k = 0, i, j, ncstatus

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

    if(present(mode)) call print_nc_info(input_file, mode)

  end subroutine nc_reader

  function nc_allocate(n, len, val1, val2, val3)

    use hamodule

    implicit none

    integer, intent(in) :: n
    integer, intent(in), dimension(n) :: len
    type(ncvalues), intent(out), dimension(:), allocatable, optional :: val1
    type(ncvalues), intent(out), dimension(:,:), allocatable, optional :: val2
    type(ncvalues), intent(out), dimension(:,:,:), allocatable, optional :: val3
    integer :: allocate_error_status
    character(nf90_max_name) :: allocate_error_message

    integer :: nc_allocate, un = 6

    select case(n)
    case(1)
      allocate( & !
        val1(len(1)), & !
        stat = allocate_error_status, & !
        errmsg = allocate_error_message & !
        )
    case(2)
       allocate( & !
         val2(len(1), len(2)), & !
         stat = allocate_error_status, & !
         errmsg = allocate_error_message & !
         )
    case(3)
       allocate( val3(len(1), len(2), len(3)), & !
         stat = allocate_error_status, & !
         errmsg = allocate_error_message & !
         )
    end select

    nc_allocate = allocate_error_status
    
    if(allocate_error_status /= 0) then
      write(un, '(a)') ''
      write(un, '(a)') 'WARNING: Failed to allocate val' // &
      number_to_string( &
        iv = int(n, 4), &
        len = num_len(iv = int(n, 4)) &
        ) // &
      ' with error status ' // &
      number_to_string( &
        iv = int(allocate_error_status, 4), &
        len = num_len(iv = int(allocate_error_status, 4)) &
        ) // ' (' //  trim(allocate_error_message) // ')'
      write(un, '(a)') ''
    end if

    return

  end function nc_allocate

end module ncmodule

