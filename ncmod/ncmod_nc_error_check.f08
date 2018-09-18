subroutine nc_error_check(check_type, ncstatus)
  use netcdf
  use hamodule
  implicit none
  character(*), intent(in) :: check_type
  integer, intent(in) :: ncstatus
  integer :: stdout = 6
  if(ncstatus /= nf90_noerr) then
    select case(check_type)
    case('nc_open')
      write(stdout, '(a)') 'Error in nc_open: '//number_to_string(ncstatus)
    case('nc_inquire')
      write(stdout, '(a)') 'Error in nc_inquire: '//number_to_string(ncstatus)
    case('nc_inq_dimid')
      write(stdout, '(a)') 'Error in nc_inq_dimid: '//number_to_string(ncstatus)
    case('nc_inquire_dimension')
      write(stdout, '(a)') 'Error in nc_inquire_dimension: '//number_to_string(ncstatus)
    case('nc_inq_varid')
      write(stdout, '(a)') 'Error in nc_inq_varid: '//number_to_string(ncstatus)
    case('nc_variable')
      write(stdout, '(a)') 'Error in nc_variable: '//number_to_string(ncstatus)
    case('nc_inq_attname')
      write(stdout, '(a)') 'Error in nc_inq_attname: '//number_to_string(ncstatus)
    case('nc_inquire_attribute')
      write(stdout, '(a)') 'Error in nc_inquire_attribute: '//number_to_string(ncstatus)
    case('nc_get_att')
      write(stdout, '(a)') 'Error in nc_get_att: '//number_to_string(ncstatus)
    case('nc_')
      write(stdout, '(a)') 'Error in nc_: '//number_to_string(ncstatus)
    end select
    write(stdout, '(a)') trim(nf90_strerror(ncstatus))
    STOP 'Stopped!'
  end if
  return
end subroutine nc_error_check
