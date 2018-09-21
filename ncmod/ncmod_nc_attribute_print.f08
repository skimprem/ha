function nc_attribute_print(attribute)
  use hamodule
  implicit none
  type(ncattributes), intent(in) :: attribute 
  integer(4) :: i
  character(10000) :: temp_string
  character(len=:), allocatable :: nc_attribute_print
  character(len=2) :: sep
  temp_string = ''
  sep = '; '
  select case(attribute%xtype)
  case(nf90_byte)
    do i = 1, attribute%len
      temp_string = trim(adjustl(temp_string))//sep//number_to_string(attribute%value_byte(i))
    end do
  case(nf90_ubyte)
  case(nf90_char)
    do i = 1, attribute%len
      temp_string(i:i) = attribute%value_char(i)
    end do
  case(nf90_short)
    do i = 1, attribute%len
      temp_string = trim(adjustl(temp_string))//sep//number_to_string(attribute%value_short(i))
    end do
  case(nf90_ushort)
  case(nf90_int)
    do i = 1, attribute%len
      temp_string = trim(adjustl(temp_string))//sep//number_to_string(attribute%value_int(i))
    end do
  case(nf90_uint)
  case(nf90_int64)
    do i = 1, attribute%len
      temp_string = trim(adjustl(temp_string))//sep//number_to_string(attribute%value_int64(i))
    end do
  case(nf90_uint64)
  case(nf90_float)
    do i = 1, attribute%len
      temp_string = trim(adjustl(temp_string))//sep//number_to_string(attribute%value_float(i))
    end do
  case(nf90_double)
    do i = 1, attribute%len
      temp_string = trim(adjustl(temp_string))//sep//number_to_string(attribute%value_double(i))
    end do
  case(nf90_string)
  end select

  !nc_attribute_print = trim(temp_string(:len_trim(temp_string)-1))
  nc_attribute_print = trim(adjustl(trim(temp_string(2:))))
  return
end function nc_attribute_print
