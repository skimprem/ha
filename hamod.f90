MODULE hamodule

  !type hacoeff

  !end type hacoeff

CONTAINS

  SUBROUTINE input_check(check_type, arg, string)

    IMPLICIT NONE

    CHARACTER(*), INTENT(in) :: check_type
    CHARACTER(*), INTENT(in) :: arg
    CHARACTER(*), INTENT(in), OPTIONAL :: string
    LOGICAL :: file_exist = .FALSE., arg_true = .FALSE.
    INTEGER(4) :: i, k = 1

    ! check types:
    !   'noarg' - parameter existence
    !   'nofile' - file existence
    !   'checkarg' - argument is true

    SELECT CASE(check_type)
    CASE('noarg')
       IF(TRIM(ADJUSTL(arg)) == '') THEN
          PRINT '(a)', 'ERROR: Do not set any option!'
          CALL print_help('stop')
       END IF
    CASE('noopt')
       IF(TRIM(ADJUSTL(arg)) == '') THEN
          PRINT '(a)', 'ERROR: Do not define parameter of option "'//TRIM(ADJUSTL(string))//'"'
          CALL print_help('stop')
       END IF
    CASE('nofile')
       INQUIRE(file=TRIM(ADJUSTL(arg)), exist=file_exist)
       IF(file_exist .EQV. .FALSE.) THEN
          PRINT '(a)', 'ERROR: No such file "'//TRIM(ADJUSTL(arg))//'"'
          CALL print_help('stop')
       END IF
    CASE('unopt')
       PRINT '(a)', 'ERROR: Option "'//TRIM(ADJUSTL(arg))//'" unrecognized!'
       CALL print_help('stop')
    CASE('charg')
       DO i = 1, len_TRIM(string)
          IF(string(i:i) == ',') THEN
             IF( TRIM(ADJUSTL(arg)) == TRIM(ADJUSTL(string(k:i-1))) ) arg_true = .TRUE.
             k = i + 1
          END IF
       END DO
       IF(TRIM(ADJUSTL(string(k:))) == TRIM(ADJUSTL(arg))) arg_true = .TRUE.
       IF(arg_true .EQV. .FALSE.) THEN
          PRINT '(a)', 'ERROR: The parameter '//TRIM(ADJUSTL(arg))//' is incorrect!'
          CALL print_help('stop')
       END IF
    END SELECT

    RETURN

  END SUBROUTINE input_check

  SUBROUTINE print_help(type_help)
    IMPLICIT NONE
    CHARACTER(*), INTENT(in), OPTIONAL :: type_help

    PRINT '(a)', 'Usage: ha [OPTION]... [FILE]...'
    PRINT '(a)', ''
    PRINT '(a)', 'Calculates the coefficients of spherical harmonics over a regular grid' 
    PRINT '(a)', 'using various algorithms'

    IF(PRESENT(type_help)) THEN
       SELECT CASE(type_help)
       CASE('stop')
          !stop 'Stopped!'
          STOP
       END SELECT
    END IF

    RETURN
  END SUBROUTINE print_help

  INTEGER(4) FUNCTION num_len(iv, rv, frmt)

    IMPLICIT NONE
    INTEGER(4), INTENT(in), OPTIONAL :: iv
    REAL, INTENT(in), OPTIONAL :: rv
    CHARACTER(*), INTENT(in), OPTIONAL :: frmt
    CHARACTER(10000) :: string

    IF(PRESENT(iv)) THEN
       IF(PRESENT(frmt)) THEN
          WRITE(string, frmt) iv
       ELSE
          WRITE(string, *) iv
       END IF
    ELSE IF(PRESENT(rv)) THEN
       IF(PRESENT(frmt)) THEN
          WRITE(string, frmt) rv
       ELSE
          WRITE(string, *) rv
       END IF
    ELSE
       string = ''
    END IF

    num_len = len_TRIM(ADJUSTL(string))

    RETURN

  END FUNCTION num_len

  FUNCTION number_to_string(iv, rv, len, frmt)

    IMPLICIT NONE
    INTEGER(4), INTENT(in), OPTIONAL :: iv
    REAL, INTENT(in), OPTIONAL :: rv
    INTEGER(4), INTENT(in) :: len
    CHARACTER(*), INTENT(in), OPTIONAL :: frmt
    CHARACTER(10000) :: string
    CHARACTER(len=len) :: number_to_string

    IF(PRESENT(frmt)) THEN
       IF(PRESENT(iv)) THEN
          WRITE(string, frmt) iv
       ELSE IF(PRESENT(rv)) THEN
          WRITE(string, frmt) rv
       ELSE
          string = ''
       END IF
    ELSE
       IF(PRESENT(iv)) THEN
          WRITE(string, *) iv
       ELSE IF(PRESENT(rv)) THEN
          WRITE(string, *) rv
       ELSE
          string = ''
       END IF
    END IF

    number_to_string = TRIM(ADJUSTL(string))

    RETURN

  END FUNCTION number_to_string

END MODULE hamodule
