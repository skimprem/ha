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

    integer(kind=1), allocatable :: byte_1(:), byte_2(:,:), byte_3(:,:,:)
    integer(kind=2), allocatable :: short_1(:), short_2(:,:), short_3(:,:,:)
    integer(kind=4), allocatable :: int_1(:), int_2(:,:), int_3(:,:,:)
    integer(kind=8), allocatable :: int64_1(:), int64_2(:,:), int64_3(:,:,:)
    real(kind=4), allocatable :: float_1(:), float_2(:,:), float_3(:,:,:)
    real(kind=8), allocatable :: double_1(:), double_2(:,:), double_3(:,:,:)
    character(kind=1, len=1), allocatable ::&
      char_1(:), char_2(:,:), char_3(:,:,:)
    character(kind=1, len=nf90_max_name), allocatable ::&
      string_1(:), string_2(:,:), string_3(:,:,:)

  end type ncvalues

  type ncattributes
    character(nf90_max_name) ::&
        name
    integer ::&
        xtype,&
        len,&
        attnum 
    type(ncvalues) ::&
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
    type(ncvalues) :: value

  end type ncvariables

  type ncfile
    character(len=:), allocatable :: path
    character(len=:), allocatable ::& !
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
    type(ncvalues), intent(out) :: value
    integer :: un = 6

    select case(xtype)
    case(0)
      ! 0
      write(un, '(a)') nc_xtype_info(xtype)//' not set'
      stop
    case(nf90_byte)
      ! NC_BYTE: 8-bit signed integer
      allocate( value%byte_1(len) )
      call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = value%byte_1(1)&
            )&
          )
    case(nf90_ubyte)
      ! NC_UBYTE: 8-bit unsigned integer
      write(un, '(a)') nc_xtype_info(xtype)//' not set'
      stop
    case(nf90_char)
      ! NC_CHAR: 8-bit character byte
      allocate( value%char_1(len) )
      call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = value%char_1(1)&
            )&
          )
    case(nf90_short)
      ! NC_SHORT: 16-bit signed integer
      allocate( value%short_1(len) )
      call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = value%short_1&
            )&
          )
    case(nf90_ushort)
      ! NC_USHORT: 16-bit unsigned integer
      write(un, '(a)') nc_xtype_info(xtype)//' not set'
      stop
    case(nf90_int)
      ! NC_int: (NC_LONG): 32-bit signed integer
      allocate( value%int_1(len) )
      call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = value%int_1&
            )&
          )
    case(nf90_uint)
      ! NC_Uint: 32-bit unsigned integer
      write(un, '(a)') nc_xtype_info(xtype)//' not set'
      stop
    case(nf90_int64)
      ! NC_int64: 64-bit signed integer
      allocate( value%int64_1(len) )
      call nc_error_check(&
           'nc_get_att',&
           nf90_get_att(&
             ncid = ncid,&
             varid = varid,&
             name = name,&
             values = value%int64_1&
             )&
           )
    case(nf90_uint64)
      ! NC_Uint64: 64-bit unsigned integer
      write(un, '(a)') nc_xtype_info(xtype)//' not set'
      stop
    case(nf90_float)
      ! NC_FLOAT: 32-bit floating point
      allocate( value%float_1(len) )
      call nc_error_check(&
           'nc_get_att',&
           nf90_get_att(&
             ncid = ncid,&
             varid = varid,&
             name = name,&
             values = value%float_1&
             )&
           )
    case(nf90_double)
      ! NC_double: 64-bit floating point
      allocate( value%double_1(len) )
      call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = value%double_1&
            )&
          )
    case(nf90_string)
      ! NC_STRING: variable length character string
      allocate( value%string_1(len) )
      call nc_error_check(&
          'nc_get_att',&
          nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = value%string_1(1)&
            )&
          )
    end select

  end subroutine get_att_xtype

  subroutine get_var_xtype(&
       ncid,& !
       varid,& !
       xtype,& !
       ndims,& !
       len,&
       value) !

    use hamodule

    implicit none
    integer, intent(in) :: ncid, varid, xtype, ndims
    integer, intent(in), dimension(ndims) :: len
    integer :: allocate_status, un = 6, i, j, k
    type(ncvalues), intent(out) :: value

    select case(xtype)
    case(nf90_byte)
      select case(ndims)
      case(1)
        allocate( value%byte_1(len(1)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%byte_1&
               )&
             )
      case(2)
        allocate( value%byte_2(len(1), len(2)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%byte_2&
               )&
              )
      case(3)
        allocate( value%byte_3(len(1), len(2), len(3)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%byte_3&
               )&
             )
      end select
    case(nf90_ubyte)
      write(un, '(a)') nc_xtype_info(xtype)//' not set'
      stop 
    case(nf90_char)
      select case(ndims)
      case(1)
        allocate( value%char_1(len(1)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%char_1&
               )&
             )
      case(2)
        allocate( value%char_2(len(1), len(2)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%char_2&
               )&
              )
      case(3)
        allocate( value%char_3(len(1), len(2), len(3)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%char_3&
               )&
             )
      end select
    case(nf90_short)
      select case(ndims)
      case(1)
        allocate( value%short_1(len(1)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%short_1&
               )&
             )
      case(2)
        allocate( value%short_2(len(1), len(2)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%short_2&
               )&
              )
      case(3)
        allocate( value%short_3(len(1), len(2), len(3)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%short_3&
               )&
             )
      end select
    case(nf90_ushort)
      write(un, '(a)') nc_xtype_info(xtype)//' not set'
      stop 
    case(nf90_int)
      select case(ndims)
      case(1)
        allocate( value%int_1(len(1)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%int_1&
               )&
             )
      case(2)
        allocate( value%int_2(len(1), len(2)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%int_2&
               )&
             )
      case(3)
        allocate( value%int_3(len(1), len(2), len(3)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%int_3&
               )&
             )
      end select
    case(nf90_uint)
      write(un, '(a)') nc_xtype_info(xtype)//' not set'
      stop 
    case(nf90_int64)
      select case(ndims)
      case(1)
        allocate( value%int64_1(len(1)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%int64_1&
               )&
             )
      case(2)
        allocate( value%int64_2(len(1), len(2)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%int64_2&
               )&
             )
      case(3)
        allocate( value%int64_3(len(1), len(2), len(3)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%int64_3&
               )&
             )
      end select
    case(nf90_uint64)
      write(un, '(a)') nc_xtype_info(xtype)//' not set'
      stop 
    case(nf90_float)
      select case(ndims)
      case(1)
        allocate( value%float_1(len(1)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%float_1&
               )&
             )
      case(2)
        allocate( value%float_2(len(1), len(2)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%float_2&
               )&
             )
      case(3)
        allocate( value%float_3(len(1), len(2), len(3)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%float_3&
               )&
             )
      end select
    case(nf90_double)
      select case(ndims)
      case(1)
        allocate( value%double_1(len(1)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%double_1&
               )&
             )
      case(2)
        allocate( value%double_2(len(1), len(2)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%double_2&
               )&
             )
      case(3)
        allocate( value%double_3(len(1), len(2), len(3)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%double_3&
               )&
             )
      end select
    case(nf90_string)
      select case(ndims)
      case(1)
        allocate( value%string_1(len(1)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%string_1&
               )&
             )
      case(2)
        allocate( value%string_2(len(1), len(2)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%string_2&
               )&
             )
      case(3)
        allocate( value%string_3(len(1), len(2), len(3)) )
        call nc_error_check(&
             'nc_get_var',&
             nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = value%string_3&
               )&
             )
      end select
    end select

  end subroutine get_var_xtype

  function nc_xtype_info(xtype)

    use hamodule

    implicit none

    integer, intent(in) :: xtype
    character(kind=1, len=:), allocatable :: nc_xtype_info
    character(kind=1, len=:), allocatable :: xtype_name

    select case(xtype)
    case(nf90_byte)
      xtype_name = 'byte (8-bit signed integer)'
    case(nf90_ubyte)
      xtype_name = 'ubyte (8-bit unsigned integer)' 
    case(nf90_char)
      xtype_name = 'char (8-bit character byte)'
    case(nf90_short)
      xtype_name = 'short (16-bit signed integer)'
    case(nf90_ushort)
      xtype_name = 'ushort (16-bit unsigned integer)'
    case(nf90_int)
      xtype_name = 'int (32-bit signed integer)'
    case(nf90_uint)
      xtype_name = 'uint (32-bit unsigned integer)'
    case(nf90_int64)
      xtype_name = 'int64 (64-bit signed integer)'
    case(nf90_uint64)
      xtype_name = 'uint64 (64-bit signed integer)'
    case(nf90_float)
      xtype_name = 'float (32-bit floating point)'
    case(nf90_double)
      xtype_name = 'double (64-bit floating point)'
    case(nf90_string)
      xtype_name = 'string (variable length character string)'
    end select

    nc_xtype_info = xtype_name

    return

  end function nc_xtype_info

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
               'Error in nc_open: '//number_to_string(ncstatus)
       case('nc_inquire')
          write(un, '(a)')&
               'Error in nc_inquire: '//number_to_string(ncstatus)
       case('nc_inq_dimid')
          write(un, '(a)')&
               'Error in nc_inq_dimid: '//number_to_string(ncstatus)
       case('nc_inquire_dimension')
          write(un, '(a)')&
               'Error in nc_inquire_dimension: '//number_to_string(ncstatus)
       case('nc_inq_varid')
          write(un, '(a)')&
               'Error in nc_inq_varid: '//number_to_string(ncstatus)
       case('nc_variable')
          write(un, '(a)')&
               'Error in nc_variable: '//number_to_string(ncstatus)
       case('nc_inq_attname')
          write(un, '(a)')&
               'Error in nc_inq_attname: '//number_to_string(ncstatus)
       case('nc_inquire_attribute')
          write(un, '(a)')&
               'Error in nc_inquire_attribute: '//number_to_string(ncstatus)
       case('nc_get_att')
          write(un, '(a)')&
               'Error in nc_get_att: '//number_to_string(ncstatus)
       case('nc_')
          write(un, '(a)')&
               'Error in nc_: '//number_to_string(ncstatus)
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

    if(type_info == 'viewinfo' .or. type_info == 'view') then
       write(un, '(a)') 'NetCDF version: '//trim(nf90_inq_libvers())
       write(un, '(a)') 'nc file info:'
       write(un, '(2x,a)') '├─ ncid: '//number_to_string(nc_file%ncid)
       write(un, '(2x,a)') '├─ path: '//trim(adjustl(nc_file%path))
       write(un, '(2x,a)') '├─ mode: '//number_to_string(nc_file%cmode)
       write(un, '(2x,a)') '├─ ndimensions: '//number_to_string(nc_file%ndimensions)
       do i = 1, nc_file%ndimensions - 1
       write(un, '(2x,a)') '│  ├─ '//trim(nc_file%dimension(i)%name)//' = '//number_to_string(nc_file%dimension(i)%len)
       end do
       write(un, '(2x,a)') '│  └─ '//trim(nc_file%dimension(i)%name)//' = '//number_to_string(nc_file%dimension(i)%len)
       write(un, '(2x,a)') '├─ nVariables: '//number_to_string(nc_file%nvariables)
       do i = 1, nc_file%nvariables - 1
          write(un, '(2x,a)') '│  ├─ '//trim(nc_file%variable(i)%name)//':'
          write(un, '(2x,a)') '│  │  ├─ '//'xtype: '//nc_xtype_info(nc_file%variable(i)%xtype)
          write(un, '(2x,a)', advance = 'no') '│  │  ├─ '//'dimid: '
          do j = 1, nc_file%variable(i)%ndims
          write(un, '(a)', advance = 'no') number_to_string(nc_file%variable(i)%dimids(j))//'; '
          end do
          write(un, *)
          write(un, '(2x,a)') '│  │  └─ '//'natts: '//number_to_string(nc_file%variable(i)%natts)
          do j = 1, nc_file%variable(i)%natts - 1
             select case(nc_file%variable(i)%attribute(j)%xtype)
             case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
               write(un, '(2x,a)') '│  │     ├─ '//trim(nc_file%variable(i)%attribute(j)%name)//' = '
               do k = 1, nc_file%variable(i)%attribute(j)%len - 1
                  write(un, '(2x,a)') '│  │     │  ├─ '//nc_value_print(value = nc_file%variable(i)%attribute(j)%value,&
                  xtype = nc_file%variable(i)%attribute(j)%xtype, ndims = 1, i = k)
               end do
               write(un, '(2x,a)') '│  │     │  └─ '//nc_value_print(value = nc_file%variable(i)%attribute(j)%value,&
               xtype = nc_file%variable(i)%attribute(j)%xtype, ndims = 1, i = k)
             case(nf90_char)
               write(un, '(2x,a)', advance = 'no') '│  │     ├─ '//trim(nc_file%variable(i)%attribute(j)%name)//' = "'
               do k = 1, nc_file%variable(i)%attribute(j)%len
                 write(un, '(a)', advance = 'no') nc_file%variable(i)%attribute(j)%value%char_1(k)
               end do
               write(un, '(a)') '"'
             end select
          end do
          select case(nc_file%variable(i)%attribute(j)%xtype)
          case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
            write(un, '(2x,a)', advance = 'no') '│  │     └─ '//trim(nc_file%variable(i)%attribute(j)%name)//' = '
            do k = 1, nc_file%variable(i)%attribute(j)%len
               write(un, '(a)', advance = 'no') nc_value_print(value = nc_file%variable(i)%attribute(j)%value,&
               xtype = nc_file%variable(i)%attribute(j)%xtype, ndims = 1, i = k)//'; '
            end do
            write(un, *)
          case(nf90_char)
            write(un, '(2x,a)', advance = 'no') '│  │     └─ '//trim(nc_file%variable(i)%attribute(j)%name)//' = "'
            do k = 1, nc_file%variable(i)%attribute(j)%len
              write(un, '(a)', advance = 'no') nc_file%variable(i)%attribute(j)%value%char_1(k)
            end do
            write(un, '(a)') '"'
          end select
       end do
       write(un, '(2x,a)') '│  └─ '//trim(nc_file%variable(i)%name)//':'
       write(un, '(2x,a)') '│     ├─ '//'xtype: '//nc_xtype_info(nc_file%variable(i)%xtype)
       write(un, '(2x,a)', advance = 'no') '│     ├─ '//'dimid: '
       do j = 1, nc_file%variable(i)%ndims
         write(un, '(a)', advance = 'no') number_to_string(nc_file%variable(i)%dimids(j))//'; '
       end do
       write(un, *)
       write(un, '(2x,a)') '│     └─ '//'natts: '//number_to_string(nc_file%variable(i)%natts)
       do j = 1, nc_file%variable(i)%natts - 1
          select case(nc_file%variable(i)%attribute(j)%xtype)
          case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
            write(un, '(2x,a)', advance = 'no') '│        ├─ '//trim(nc_file%variable(i)%attribute(j)%name)//' = '
            do k = 1, nc_file%variable(i)%attribute(j)%len
               write(un, '(a)', advance = 'no') nc_value_print(nc_file%variable(i)%attribute(j)%value,&
               nc_file%variable(i)%attribute(j)%xtype, 1, k)//'; '
            end do
            write(un, *)
          case(nf90_char)
            write(un, '(2x,a)', advance = 'no') '│        ├─ '//trim(nc_file%variable(i)%attribute(j)%name)//' = "'
            do k = 1, nc_file%variable(i)%attribute(j)%len
              write(un, '(a)', advance = 'no') &
                nc_file%variable(i)%attribute(j)%value%char_1(k)
            end do
            write(un, '(a)') '"'
          end select
       end do
       select case(nc_file%variable(i)%attribute(j)%xtype)
       case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
         write(un, '(2x,a)', advance = 'no') '│        └─ '//trim(nc_file%variable(i)%attribute(j)%name)//' = '
         do k = 1, nc_file%variable(i)%attribute(j)%len
            write(un, '(a)', advance = 'no') nc_value_print(nc_file%variable(i)%attribute(j)%value,&
            nc_file%variable(i)%attribute(j)%xtype, 1, k)//'; '
         end do
         write(un, *)
       case(nf90_char)
         write(un, '(2x,a)', advance = 'no') '│        └─ '//trim(nc_file%variable(i)%attribute(j)%name)//' = "'
         do k = 1, nc_file%variable(i)%attribute(j)%len
           write(un, '(a)', advance = 'no') &
             nc_file%variable(i)%attribute(j)%value%char_1(k)
         end do
         write(un, '(a)') '"'
       end select

       write(un, '(2x,a)') '├─ nAttributes: '//number_to_string(nc_file%nattributes)
       do i = 1, nc_file%nattributes - 1
         select case(nc_file%attribute(i)%xtype)
         case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
           write(un, '(2x,a)') '│  ├─ '//trim(nc_file%attribute(i)%name)//' = '//&
             nc_value_print(nc_file%attribute(i)%value, nc_file%attribute(i)%xtype, 1, 1)
         case(nf90_char)
           write(un, '(2x,a)', advance = 'no') '│  ├─ '//trim(nc_file%attribute(i)%name)//' = "'
           do j = 1, nc_file%attribute(i)%len 
             write(un, '(a)', advance = 'no') nc_file%attribute(i)%value%char_1(j)
           end do
           write(un, '(a)') '"'
         end select
       end do
       select case(nc_file%attribute(i)%xtype)
       case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
         write(un, '(2x,a)') '│  └─ '//trim(nc_file%attribute(i)%name)//' = '//&
           nc_value_print(nc_file%attribute(i)%value, nc_file%attribute(i)%xtype, 1, 1)
       case(nf90_char)
         write(un, '(2x,a)', advance = 'no') '│  └─ '//trim(nc_file%attribute(i)%name)//' = "'
         do j = 1, nc_file%attribute(i)%len 
           write(un, '(a)', advance = 'no') nc_file%attribute(i)%value%char_1(j)
         end do
         write(un, '(a)') '"'
       end select

       write(un, '(2x,a)') '├─ unlimitedDimid: '//&
            number_to_string(nc_file%unlimiteddimid)
       write(un, '(2x,a)') '└─ formatNum: '//&
            number_to_string(nc_file%formatnum)
    end if

    if(type_info == 'viewdata' .or. type_info == 'view') then
      write(un, '(a)') 
      k = 1
      do i = 1, nc_file%nvariables
        write(un, '(a)', advance = 'no') trim(nc_file%variable(i)%name)//'; '
      end do
      write(un, *)
      do i = 1, nc_file%dimension(1)%len 
        do j = 1,  nc_file%dimension(2)%len
          write(un, '(a)')&
            nc_value_print(value = nc_file%variable(1)%value,&
                           xtype = nc_file%variable(1)%xtype,&
                           ndims = nc_file%variable(1)%ndims,&
                           i = i)//'; '//&
            nc_value_print(value = nc_file%variable(2)%value,&
                           xtype = nc_file%variable(2)%xtype,&
                           ndims = nc_file%variable(2)%ndims,&
                           i = j)//'; '//&
            nc_value_print(value = nc_file%variable(3)%value,&
                           xtype = nc_file%variable(3)%xtype,&
                           ndims = nc_file%variable(3)%ndims,&
                           i = i, j = j)
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

       call get_var_xtype(&
           ncid = input_file%ncid,&
           varid = input_file%variable(i)%varid,&
           xtype = input_file%variable(i)%xtype,&
           ndims = input_file%variable(i)%ndims,&
           len = input_file%variable(i)%len,&
           value = input_file%variable(i)%value&
           )
    end do

    if(present(mode)) call print_nc_info(input_file, mode)

  end subroutine nc_reader

  function nc_value_print(value, xtype, ndims, i, j, k)

    use hamodule

    implicit none

    type(ncvalues), intent(in) :: value
    integer(4), intent(in) :: xtype, ndims, i
    integer(4), intent(in), optional :: j, k
    character(:), allocatable :: nc_value_print

    !if(present(j) .eqv. .false. .and. present(k) .eqv. .false.) then
      !if(ndims /= 1) stop 'error in nc_value_print() function: the value of ndims&
        !must be equal to 1'
    !else if(present(j) .eqv. .true. .and. present(k) .eqv. .false.) then
      !if(ndims /= 2) stop 'error in nc_value_print() function: the value of ndims&
        !must be equal to 2'
    !else if(present(j) .eqv. .true. .and. present(k) .eqv. .true.) then
      !if(ndims /= 3) stop 'error in nc_value_print() function: the value of ndims&
        !must be equal to 3'
    !else
      !stop 'error in nc_value_print() function:'
    !end if

    select case(xtype)
    case(nf90_byte)
      select case(ndims)
      case(1)
        nc_value_print = number_to_string(value%byte_1(i))
      case(2)
        nc_value_print = number_to_string(value%byte_2(i, j))
      case(3)
        nc_value_print = number_to_string(value%byte_3(i, j, k))
      end select
    case(nf90_ubyte)
    case(nf90_char)
      select case(ndims)
      case(1)
        nc_value_print = value%char_1(i)
      case(2)
        nc_value_print = value%char_2(i, j)
      case(3)
        nc_value_print = value%char_3(i, j, k)
      end select
    case(nf90_short)
      select case(ndims)
      case(1)
        nc_value_print = number_to_string(value%short_1(i))
      case(2)
        nc_value_print = number_to_string(value%short_2(i, j))
      case(3)
        nc_value_print = number_to_string(value%short_3(i, j, k))
      end select
    case(nf90_ushort)
    case(nf90_int)
      select case(ndims)
      case(1)
        nc_value_print = number_to_string(value%int_1(i))
      case(2)
        nc_value_print = number_to_string(value%int_2(i, j))
      case(3)
        nc_value_print = number_to_string(value%int_3(i, j, k))
      end select
    case(nf90_uint)
    case(nf90_int64)
      select case(ndims)
      case(1)
        nc_value_print = number_to_string(value%int64_1(i))
      case(2)
        nc_value_print = number_to_string(value%int64_2(i, j))
      case(3)
        nc_value_print = number_to_string(value%int64_3(i, j, k))
      end select
    case(nf90_uint64)
    case(nf90_float)
      select case(ndims)
      case(1)
        nc_value_print = number_to_string(value%float_1(i))
      case(2)
        nc_value_print = number_to_string(value%float_2(i, j))
      case(3)
        nc_value_print = number_to_string(value%float_3(i, j, k))
      end select
    case(nf90_double)
      select case(ndims)
      case(1)
        nc_value_print = number_to_string(value%double_1(i))
      case(2)
        nc_value_print = number_to_string(value%double_2(i, j))
      case(3)
        nc_value_print = number_to_string(value%double_3(i, j, k))
      end select
    case(nf90_string)
      select case(ndims)
      case(1)
        nc_value_print = trim(adjustl(value%string_1(i)))
      case(2)
        nc_value_print = trim(adjustl(value%string_2(i, j)))
      case(3)
        nc_value_print = trim(adjustl(value%string_3(i, j, k)))
      end select
    end select

    return

  end function nc_value_print

  !function nc_allocate(n, len, val1, val2, val3)

    !use hamodule

    !implicit none

    !interface num_len
      !module procedure num_len_int_2, num_len_int_4, num_len_real_4, num_len_real_8
    !end interface num_len

    !integer, intent(in) :: n
    !integer, intent(in), dimension(n) :: len
    !type(ncvalues), intent(out), dimension(:), allocatable, optional :: val1
    !type(ncvalues), intent(out), dimension(:,:), allocatable, optional :: val2
    !type(ncvalues), intent(out), dimension(:,:,:), allocatable, optional :: val3
    !integer :: allocate_error_status
    !character(nf90_max_name) :: allocate_error_message

    !integer :: nc_allocate, un = 6

    !select case(n)
    !case(1)
      !allocate( & !
        !val1(len(1)), & !
        !stat = allocate_error_status, & !
        !errmsg = allocate_error_message & !
        !)
    !case(2)
       !allocate( & !
         !val2(len(1), len(2)), & !
         !stat = allocate_error_status, & !
         !errmsg = allocate_error_message & !
         !)
    !case(3)
       !allocate( val3(len(1), len(2), len(3)), & !
         !stat = allocate_error_status, & !
         !errmsg = allocate_error_message & !
         !)
    !end select

    !nc_allocate = allocate_error_status
    
    !if(allocate_error_status /= 0) then
      !write(un, '(a)') ''
      !write(un, '(a)') 'WARNING: Failed to allocate val' // &
      !number_to_string( &
        !iv = int(n, 4), &
        !len = num_len(n) &
        !) // &
      !' with error status ' // &
      !number_to_string( &
        !iv = int(allocate_error_status, 4), &
        !len = num_len(allocate_error_status) &
        !) // ' (' //  trim(allocate_error_message) // ')'
      !write(un, '(a)') ''
    !end if

    !return

  !end function nc_allocate

end module ncmodule

