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
