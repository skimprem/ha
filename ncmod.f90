MODULE ncmodule

  USE netcdf

  TYPE ncdimensions
     CHARACTER(nf90_max_name) ::&
          name
     INTEGER ::&
          dimid,&
          len
  END TYPE ncdimensions

  TYPE ncvalues
     INTEGER(2) :: short
     INTEGER(4) :: int
     INTEGER(8) :: int64
     REAL(4) :: float
     REAL(8) :: DOUBLE
     CHARACTER(nf90_max_name) :: char
  END TYPE ncvalues

  TYPE ncattributes
     CHARACTER(nf90_max_name) ::&
          name
     INTEGER ::&
          xtype,&
          len,&
          attnum 
     TYPE(ncvalues), DIMENSION(:), ALLOCATABLE ::&
          VALUE
  END TYPE ncattributes

  TYPE ncxtypes
     CHARACTER(nf90_max_name) ::&
          name
     INTEGER ::&
          TYPE,&
          size
  END TYPE ncxtypes

  TYPE ncvariables
     CHARACTER(nf90_max_name) ::&
          name
     INTEGER ::&
          varid,&
          xtype,&
          ndims,&
          natts
     INTEGER, DIMENSION(:), ALLOCATABLE ::&
          dimids
     INTEGER, DIMENSION(:), ALLOCATABLE ::&
          len
     TYPE(ncattributes), DIMENSION(:), ALLOCATABLE :: attribute
     TYPE(ncvalues), DIMENSION(:), ALLOCATABLE :: val1
     TYPE(ncvalues), DIMENSION(:,:), ALLOCATABLE :: val2
     TYPE(ncvalues), DIMENSION(:,:,:), ALLOCATABLE :: val3
  END TYPE ncvariables

  TYPE ncfile
     CHARACTER(1000) :: path
     CHARACTER(nf90_max_name) ::& !
          name,&
          title,&
          history,&
          conventions
     INTEGER ::& !
          cmode,& !
          ncid,& !
          ndimensions,& !
          nvariables,& !
          nattributes,& !
          unlimiteddimid,& !
          formatnum !
     TYPE(ncdimensions), DIMENSION(:), ALLOCATABLE :: DIMENSION
     TYPE(ncvariables), DIMENSION(:), ALLOCATABLE :: variable
     TYPE(ncattributes), DIMENSION(:), ALLOCATABLE :: attribute
  END TYPE ncfile

CONTAINS

  SUBROUTINE get_att_xtype(&
       ncid,& !
       varid,& !
       xtype,& !
       name,& !
       len,& !
       VALUE) !

    USE netcdf

    IMPLICIT NONE

    INTEGER, INTENT(in) :: ncid, varid, xtype, len
    CHARACTER(*), INTENT(in) :: name
    TYPE(ncvalues), INTENT(out), DIMENSION(:), ALLOCATABLE :: VALUE

    ALLOCATE( VALUE(len) )

    SELECT CASE(xtype)
    CASE(0)
       ! 0
    CASE(nf90_byte)
       ! NC_BYTE: 8-bit signed integer
    CASE(nf90_ubyte)
       ! NC_UBYTE: 8-bit unsigned integer
    CASE(nf90_char)
       ! NC_CHAR: 8-bit character byte
       DEALLOCATE( VALUE )
       ALLOCATE( VALUE(1) )
       CALL nc_error_check(&
            'nc_get_att',&
            nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = VALUE(1)%char&
            )&
            )
    CASE(nf90_short)
       ! NC_SHORT: 16-bit signed integer
       CALL nc_error_check(&
            'nc_get_att',&
            nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = VALUE%short&
            )&
            )
    CASE(nf90_ushort)
       ! NC_USHORT: 16-bit unsigned integer
    CASE(nf90_int)
       ! NC_INT: (NC_LONG): 32-bit signed integer
       CALL nc_error_check(&
            'nc_get_att',&
            nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = VALUE%int&
            )&
            )
    CASE(nf90_uint)
       ! NC_UINT: 32-bit unsigned integer
    CASE(nf90_int64)
       ! NC_INT64: 64-bit signed integer
       CALL nc_error_check(&
            'nc_get_att',&
            nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = VALUE%int64&
            )&
            )
    CASE(nf90_uint64)
       ! NC_UINT64: 64-bit unsigned integer
    CASE(nf90_float)
       ! NC_FLOAT: 32-bit floating point
       CALL nc_error_check(&
            'nc_get_att',&
            nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = VALUE%float&
            )&
            )
    CASE(nf90_double)
       ! NC_DOUBLE: 64-bit floating point
       CALL nc_error_check(&
            'nc_get_att',&
            nf90_get_att(&
            ncid = ncid,&
            varid = varid,&
            name = name,&
            values = VALUE%double&
            )&
            )
    CASE(nf90_string)
       ! NC_STRING: variable length character string
    END SELECT

  END SUBROUTINE get_att_xtype

  SUBROUTINE get_var_xtype(&
       ncid,& !
       varid,& !
       xtype,& !
       ndims,& !
       len,& !
       val1,& !
       val2,& !
       val3) !

    IMPLICIT NONE
    INTEGER, INTENT(in) :: ncid, varid, xtype, ndims
    INTEGER, INTENT(in), DIMENSION(ndims) :: len
    TYPE(ncvalues), INTENT(out), DIMENSION(:), ALLOCATABLE, OPTIONAL :: val1
    TYPE(ncvalues), INTENT(out), DIMENSION(:,:), ALLOCATABLE, OPTIONAL :: val2
    TYPE(ncvalues), INTENT(out), DIMENSION(:,:,:), ALLOCATABLE, OPTIONAL :: val3

    SELECT CASE(ndims)
    CASE(1)
       ALLOCATE( val1(LEN(1)) )
    CASE(2)
       ALLOCATE( val2(LEN(1), LEN(2)) )
    CASE(3)
       ALLOCATE( val3(LEN(1), LEN(2), LEN(3)) )
    END SELECT

    SELECT CASE(xtype)
    CASE(nf90_byte)
    CASE(nf90_ubyte)
    CASE(nf90_char)
    CASE(nf90_short)
    CASE(nf90_ushort)
    CASE(nf90_int)
    CASE(nf90_uint)
    CASE(nf90_int64)
    CASE(nf90_uint64)
    CASE(nf90_float)
       SELECT CASE(ndims)
       CASE(1)
          CALL nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = val1%float&
               )&
               )
       CASE(2)
          CALL nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = val2%float&
               )&
               )
       CASE(3)
          CALL nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = val3%float&
               )&
               )

       END SELECT
    CASE(nf90_double)
       SELECT CASE(ndims)
       CASE(1)
          CALL nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = val1%double&
               )&
               )
       CASE(2)
          CALL nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = val2%double&
               )&
               )
       CASE(3)
          CALL nc_error_check(&
               'nc_get_var',&
               nf90_get_var(&
               ncid = ncid,&
               varid = varid,&
               values = val3%double&
               )&
               )
       END SELECT
    CASE(nf90_string)
    END SELECT

  END SUBROUTINE get_var_xtype

  SUBROUTINE nc_error_check(check_type, ncstatus)

    USE netcdf
    USE hamodule

    IMPLICIT NONE

    CHARACTER(*), INTENT(in) :: check_type
    INTEGER, INTENT(in) :: ncstatus

    IF(ncstatus /= nf90_noerr) THEN
       SELECT CASE(check_type)
       CASE('nc_open')
          PRINT '(a)',&
               'Error in nc_open: '//&
               number_to_string(iv = INT(ncstatus, 4), len = num_len(iv = INT(ncstatus, 4)))
       CASE('nc_inquire')
          PRINT '(a)',&
               'Error in nc_inquire: '//&
               number_to_string(iv = INT(ncstatus, 4), len = num_len(iv = INT(ncstatus, 4)))
       CASE('nc_inq_dimid')
          PRINT '(a)',&
               'Error in nc_inq_dimid: '//&
               number_to_string(iv = INT(ncstatus, 4), len = num_len(iv = INT(ncstatus, 4)))
       CASE('nc_inquire_dimension')
          PRINT '(a)',&
               'Error in nc_inquire_dimension: '//&
               number_to_string(iv = INT(ncstatus, 4), len = num_len(iv = INT(ncstatus, 4)))
       CASE('nc_inq_varid')
          PRINT '(a)',&
               'Error in nc_inq_varid: '//&
               number_to_string(iv = INT(ncstatus, 4), len = num_len(iv = INT(ncstatus, 4)))
       CASE('nc_variable')
          PRINT '(a)',&
               'Error in nc_variable: '//&
               number_to_string(iv = INT(ncstatus, 4), len = num_len(iv = INT(ncstatus, 4)))
       CASE('nc_inq_attname')
          PRINT '(a)',&
               'Error in nc_inq_attname: '//&
               number_to_string(iv = INT(ncstatus, 4), len = num_len(iv = INT(ncstatus, 4)))
       CASE('nc_inquire_attribute')
          PRINT '(a)',&
               'Error in nc_inquire_attribute: '//&
               number_to_string(iv = INT(ncstatus, 4), len = num_len(iv = INT(ncstatus, 4)))
       CASE('nc_get_att')
          PRINT '(a)',&
               'Error in nc_get_att: '//&
               number_to_string(iv = INT(ncstatus, 4), len = num_len(iv = INT(ncstatus, 4)))
       CASE('nc_')
          PRINT '(a)',&
               'Error in nc_: '//&
               number_to_string(iv = INT(ncstatus, 4), len = num_len(iv = INT(ncstatus, 4)))

       END SELECT
       PRINT '(a)', TRIM(nf90_strerror(ncstatus))
       STOP 'Stopped!'
    END IF

    RETURN

  END SUBROUTINE nc_error_check

  SUBROUTINE print_nc_info(nc_file, type_info)

    USE hamodule

    IMPLICIT NONE
    TYPE(ncfile), INTENT(in) :: nc_file
    CHARACTER(*), INTENT(in), OPTIONAL :: type_info
    INTEGER :: i, j, k, l, m

    ! type_info:
    !   view
    !   viewdata
    !   viewinfo

    IF(type_info == 'viewinfo' .OR. type_info == 'view') THEN
       PRINT '(a)', 'NetCDF version: '//TRIM(nf90_inq_libvers())
       PRINT '(a)', 'nc file info:'
       PRINT '(2x,a)', 'ncid: '//&
            number_to_string(iv = INT(nc_file%ncid, 4),&
            len = num_len(iv = INT(nc_file%ncid, 4)))
       PRINT '(2x,a)', 'path: '//TRIM(ADJUSTL(nc_file%path))
       PRINT '(2x,a)', 'mode: '//&
            number_to_string(iv = INT(nc_file%cmode, 4),&
            len = num_len(INT(nc_file%cmode, 4)))
       PRINT '(2x,a)', 'nDimensions: '//&
            number_to_string(iv = INT(nc_file%ndimensions, 4),&
            len = num_len(INT(nc_file%ndimensions, 4)))
       DO i = 1, nc_file%ndimensions
          PRINT '(4x,a)', 'name: '//TRIM(nc_file%DIMENSION(i)%name)
          PRINT '(6x,a)', 'len: '//&
               number_to_string(iv = INT(nc_file%DIMENSION(i)%len, 4),&
               len = num_len(iv = INT(nc_file%DIMENSION(i)%len,4)))
       END DO
       PRINT '(2x,a)', 'nVariables: '//&
            number_to_string(iv = INT(nc_file%nvariables, 4),&
            len = num_len(iv = INT(nc_file%nvariables, 4)))
       DO i = 1, nc_file%nvariables
          PRINT '(4x,a)', 'name: '//TRIM(nc_file%variable(i)%name)
          PRINT '(6x,a)', 'xtype: '//&
               number_to_string(iv = INT(nc_file%variable(i)%xtype, 4),&
               len = num_len(iv = INT(nc_file%variable(i)%xtype, 4)))
          PRINT '(6x,a)', 'ndims: '//&
               number_to_string(iv = INT(nc_file%variable(i)%ndims, 4),&
               len = num_len(iv = INT(nc_file%variable(i)%ndims, 4)))
          DO j = 1, nc_file%variable(i)%ndims
             PRINT '(8x,a)', 'dimid: '//&
                  number_to_string(iv = INT(nc_file%variable(i)%dimids(j), 4),&
                  len = num_len(iv = INT(nc_file%variable(i)%dimids(j), 4)))
          END DO
          PRINT '(6x,a)', 'natts: '//&
               number_to_string(iv = INT(nc_file%variable(i)%natts, 4),&
               len = num_len(iv = INT(nc_file%variable(i)%natts, 4)))
          DO j = 1, nc_file%variable(i)%natts
             PRINT '(8x,a)', 'name: '//&
                  TRIM(nc_file%variable(i)%attribute(j)%name)
             PRINT '(10x,a)', 'xtype: '//&
                  number_to_string(iv = INT(nc_file%variable(i)%attribute(j)%xtype, 4),&
                  len = num_len(iv = INT(nc_file%variable(i)%attribute(j)%xtype, 4)))
             PRINT '(10x,a)', 'len: '//&
                  number_to_string(iv = INT(nc_file%variable(i)%attribute(j)%len, 4),&
                  len = num_len(iv = INT(nc_file%variable(i)%attribute(j)%len, 4)))

             SELECT CASE(nc_file%variable(i)%attribute(j)%xtype)
             CASE(nf90_float)
                DO k = 1, nc_file%variable(i)%attribute(j)%len
                   PRINT '(10x,a)',&
                        'value '//&
                        number_to_string(iv = INT(k, 4), len = num_len(iv = INT(k, 4)))//': '//&
                        number_to_string(rv = REAL(nc_file%variable(i)%attribute(j)%VALUE(k)%float, 4),&
                        len = num_len(rv = REAL(nc_file%variable(i)%attribute(j)%VALUE(k)%float, 4)))!,&
                   !frmt = '(f10.3)')
                END DO
             CASE(nf90_double)
                DO k = 1, nc_file%variable(i)%attribute(j)%len
                   PRINT '(10x,a)',&
                        'value '//&
                        number_to_string(iv = INT(k, 4), len = num_len(iv = INT(k, 4)))//': '//&
                        number_to_string(rv = REAL(nc_file%variable(i)%attribute(j)%VALUE(k)%DOUBLE, 4),&
                        len = num_len(rv = REAL(nc_file%variable(i)%attribute(j)%VALUE(k)%DOUBLE, 4)))!,&
                   !frmt = '(f100.3)')
                END DO
             CASE(nf90_char)
                PRINT '(10x,a)', 'value: '//&
                     TRIM(nc_file%variable(i)%attribute(j)%VALUE(1)%char)
             END SELECT
          END DO
       END DO
       PRINT '(2x,a)', 'nAttributes: '//&
            number_to_string(iv = INT(nc_file%nattributes, 4),&
            len = num_len(iv = INT(nc_file%nattributes, 4)))
       DO i = 1, nc_file%nattributes
          PRINT '(4x,a)', TRIM(nc_file%attribute(i)%name)//': '//&
               TRIM(nc_file%attribute(i)%VALUE(1)%char)
       END DO
       PRINT '(2x,a)', 'unlimitedDimid: '//&
            number_to_string(iv = INT(nc_file%unlimiteddimid, 4),&
            len = num_len(iv = INT(nc_file%unlimiteddimid, 4)))
       PRINT '(2x,a)', 'formatNum: '//&
            number_to_string(iv = INT(nc_file%formatnum, 4),&
            len = num_len(iv = INT(nc_file%formatnum, 4)))
    END IF

    IF(type_info == 'viewdata' .OR. type_info == 'view') THEN
       !print '(a)', ''
       k = 1 
       DO m = 1, nc_file%variable(k)%ndims
          DO i = 1, nc_file%variable(k)%LEN(m)
             DO l = 1, nc_file%variable(k+1)%ndims
                DO j = 1, nc_file%variable(k+1)%LEN(l)
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
                   PRINT '(a)', &
                        number_to_string(rv = REAL(nc_file%variable(k)%val1(i)%DOUBLE, 4),&
                        len = num_len(rv = REAL(nc_file%variable(k)%val1(i)%DOUBLE, 4),&
                        frmt = '(f20.2)'),&
                        frmt = '(f20.2)')//' '//&
                        number_to_string(rv = REAL(nc_file%variable(k+1)%val1(j)%DOUBLE, 4),&
                        len = num_len(rv = REAL(nc_file%variable(k+1)%val1(j)%DOUBLE, 4),&
                        frmt = '(f20.2)'),&
                        frmt = '(f20.2)')//' '//&
                        number_to_string(rv = REAL(nc_file%variable(k+2)%val2(i,j)%float, 4),&
                        len = num_len(rv = REAL(nc_file%variable(k+2)%val2(i,j)%float, 4),&
                        frmt = '(f20.4)'),&
                        frmt = '(f20.4)')
                END DO
             END DO
          END DO
       END DO
    END IF

    RETURN

  END SUBROUTINE print_nc_info

  SUBROUTINE nc_reader(input_file, mode)

    USE netcdf

    IMPLICIT NONE
    TYPE(ncfile), INTENT(inout) :: input_file
    CHARACTER(*), INTENT(in), OPTIONAL :: mode
    INTEGER :: k = 0, i, j, ncstatus

    input_file%cmode = nf90_nowrite

    CALL nc_error_check(&
         'nc_open',&
         nf90_open(&
         path = input_file%path,&
         mode = input_file%cmode,&
         ncid = input_file%ncid&
         )&
         )

    CALL nc_error_check(&
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

    ALLOCATE(&
         input_file%DIMENSION(input_file%ndimensions),&
         input_file%variable(input_file%nvariables),&
         input_file%attribute(input_file%nattributes)&
         )

    DO i = 1, input_file%nattributes

       input_file%attribute(i)%attnum = i

       CALL nc_error_check(&
            'nc_inq_attname',&
            nf90_inq_attname(&
            ncid = input_file%ncid,&
            varid = nf90_global,&
            attnum = input_file%attribute(i)%attnum,&
            name = input_file%attribute(i)%name&
            )&
            )

       CALL nc_error_check(&
            'nc_inquire_attribute',&
            nf90_inquire_attribute(&
            ncid = input_file%ncid,&
            varid = nf90_global,&
            name = input_file%attribute(i)%name,&
            xtype = input_file%attribute(i)%xtype,&
            len = input_file%attribute(i)%len&
            )&
            )

       CALL get_att_xtype(&
            ncid = input_file%ncid,&
            varid = nf90_global,&
            xtype = input_file%attribute(i)%xtype,&
            name = input_file%attribute(i)%name,&
            len = input_file%attribute(i)%len,&
            VALUE = input_file%attribute(i)%value&
            )

    END DO

    DO i = 1, input_file%ndimensions

       input_file%DIMENSION(i)%dimid = i

       CALL nc_error_check(&
            'nc_inquire_dimension',&
            nf90_inquire_dimension(&
            ncid = input_file%ncid,&
            dimid = input_file%DIMENSION(i)%dimid,&
            name = input_file%DIMENSION(i)%name,&
            len = input_file%DIMENSION(i)%len&
            )&
            )

    END DO

    DO i = 1, input_file%nvariables

       input_file%variable(i)%varid = i

       CALL nc_error_check(&
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

       ALLOCATE(&
            input_file%variable(i)%dimids(input_file%variable(i)%ndims),&
            input_file%variable(i)%attribute(input_file%variable(i)%natts)&
            )

       CALL nc_error_check(&
            'nc_inquire_variable',&
            nf90_inquire_variable(&
            ncid = input_file%ncid,&
            varid = input_file%variable(i)%varid,&
            dimids = input_file%variable(i)%dimids&
            )&
            )

       DO j = 1, input_file%variable(i)%natts

          input_file%variable(i)%attribute(j)%attnum = j

          CALL nc_error_check(&
               'nc_inq_attname',&
               nf90_inq_attname(&
               ncid = input_file%ncid,&
               varid = input_file%variable(i)%varid,&
               attnum = input_file%variable(i)%attribute(j)%attnum,&
               name = input_file%variable(i)%attribute(j)%name&
               )&
               )

          CALL nc_error_check(&
               'nc_inquire_attribute',&
               nf90_inquire_attribute(&
               ncid = input_file%ncid,&
               varid = input_file%variable(i)%varid,&
               name = input_file%variable(i)%attribute(j)%name,&
               xtype = input_file%variable(i)%attribute(j)%xtype,&
               len = input_file%variable(i)%attribute(j)%len&
               )&
               )

          CALL get_att_xtype(&
               input_file%ncid,&
               input_file%variable(i)%varid,&
               input_file%variable(i)%attribute(j)%xtype,&
               input_file%variable(i)%attribute(j)%name,&
               input_file%variable(i)%attribute(j)%len,&
               input_file%variable(i)%attribute(j)%value&
               )

       END DO

       ALLOCATE( input_file%variable(i)%LEN(input_file%variable(i)%ndims) )

       DO j = 1, input_file%variable(i)%ndims
          CALL nc_error_check(&
               'nc_inquire_dimension',&
               nf90_inquire_dimension(&
               ncid = input_file%ncid,&
               dimid = input_file%variable(i)%dimids(j),&
               len = input_file%variable(i)%LEN(j)&
               )&
               )
       END DO


       SELECT CASE(input_file%variable(i)%ndims)
       CASE(1)
          CALL get_var_xtype(&
               ncid = input_file%ncid,&
               varid = input_file%variable(i)%varid,&
               xtype = input_file%variable(i)%xtype,&
               ndims = input_file%variable(i)%ndims,&
               len = input_file%variable(i)%len,&
               val1 = input_file%variable(i)%val1&
               )
       CASE(2)
          CALL get_var_xtype(&
               ncid = input_file%ncid,&
               varid = input_file%variable(i)%varid,&
               xtype = input_file%variable(i)%xtype,&
               ndims = input_file%variable(i)%ndims,&
               len = input_file%variable(i)%len,&
               val2 = input_file%variable(i)%val2&
               )
       END SELECT

    END DO

    IF(PRESENT(mode)) CALL print_nc_info(input_file, mode)

  END SUBROUTINE nc_reader

END MODULE ncmodule

