function nc_attribute_print(attribute)
  use hamodule
  implicit none
  type(ncattributes), intent(in) :: attribute 
  integer(4) :: i, stdout
  character(1000) :: temp_string
  character(:), allocatable :: nc_attribute_print
  temp_string = ''
  select case(attribute%xtype)
  case(nf90_byte)
    write(temp_string, '(a)') (number_to_string(attribute%value_byte(i)), i = 1, attribute%len)
  case(nf90_ubyte)
  case(nf90_char)
    !write(temp_string, '(a)') (attribute%value_char(i), i = 1, attribute%len)
    do i = 1, attribute%len
      temp_string(i:i) = attribute%value_char(i)
    end do
  case(nf90_short)
    write(temp_string, '(a)') (number_to_string(attribute%value_short(i)), i = 1, attribute%len)
  case(nf90_ushort)
  case(nf90_int)
    write(temp_string, '(a)') (number_to_string(attribute%value_int(i)), i = 1, attribute%len)
  case(nf90_uint)
  case(nf90_int64)
    write(temp_string, '(a)') (number_to_string(attribute%value_int64(i)), i = 1, attribute%len)
  case(nf90_uint64)
  case(nf90_float)
    write(temp_string, '(a)') (number_to_string(attribute%value_float(i)), i = 1, attribute%len)
  case(nf90_double)
    write(temp_string, '(a)') (number_to_string(attribute%value_double(i)), i = 1, attribute%len)
  case(nf90_string)
    write(temp_string, '(a)') (attribute%value_string(i), i = 1, attribute%len)
  end select
  nc_attribute_print = trim(temp_string)
  return
end function nc_attribute_print
