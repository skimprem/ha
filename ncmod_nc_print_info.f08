subroutine nc_print_info(nc_file, output_filename)

  use hamodule

  implicit none

  type(ncfile), intent(in) :: nc_file
  character(*), intent(in) :: output_filename
  integer(4) :: un, i, j, k

  character(:), allocatable :: d, dr, r, b, frmt

  if(output_filename == 'stdout') then
    un = 6
  else
    open(newunit = un, file = output_filename)
  end if

  d = '│  ' ! down
  !d = '|  '
  !d = '  '
  dr = '├─ ' ! down right
  !dr = '|- '
  !dr = '  '
  r = '└─ ' ! right
  !r = '|- '
  !r = '  '
  b = '   ' ! blank
  !b = '  '

  write(un, '(a)') 'NetCDF version: '//trim(nf90_inq_libvers())
  write(un, '(a)') 'nc file info:'
  write(un, '(2x,a)') dr//'ncid: '//number_to_string(nc_file%ncid)
  write(un, '(2x,a)') dr//'path: '//trim(adjustl(nc_file%path))
  write(un, '(2x,a)') dr//'mode: '//number_to_string(nc_file%cmode)
  write(un, '(2x,a)') dr//'ndimensions: '//number_to_string(nc_file%ndimensions)
  ! dimensions
  do i = 1, nc_file%ndimensions - 1
    write(un, '(2x,a)')&
    d//dr//trim(nc_file%dimension(i)%name)//' = '//number_to_string(nc_file%dimension(i)%len)
  end do
  ! last dimesion
  write(un, '(2x,a)')&
  d//r//trim(nc_file%dimension(i)%name)//' = '//number_to_string(nc_file%dimension(i)%len)
  write(un, '(2x,a)') dr//'nVariables: '//number_to_string(nc_file%nvariables)
  ! variables
  do i = 1, nc_file%nvariables - 1
     write(un, '(2x,a)') d//dr//trim(nc_file%variable(i)%name)//':'
     write(un, '(2x,a)') d//d//dr//'xtype: '//nc_xtype_info(nc_file%variable(i)%xtype)
     write(un, '(2x,a)') d//d//dr//'memory size: '//&
     number_to_string(real(nc_file%variable(i)%value%mem_bits)/8/10**6,&
     frmt = '(f100.2)')//' MB'
     write(un, '(2x,a)', advance = 'no') d//d//dr//'dimid: '
     do j = 1, nc_file%variable(i)%ndims
     write(un, '(a)', advance = 'no') number_to_string(nc_file%variable(i)%dimids(j))//'; '
     end do
     write(un, *)
     write(un, '(2x,a)') d//d//r//'natts: '//number_to_string(nc_file%variable(i)%natts)
     ! attributes for variables
     do j = 1, nc_file%variable(i)%natts - 1
        select case(nc_file%variable(i)%attribute(j)%xtype)
        ! for numbers
        case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
          write(un, '(2x,a)') d//d//b//dr//trim(nc_file%variable(i)%attribute(j)%name)//' = '
          ! attribute number values 
          do k = 1, nc_file%variable(i)%attribute(j)%len - 1
             write(un, '(2x,a)')&
             d//d//b//d//dr//nc_value_print(value = nc_file%variable(i)%attribute(j)%value,&
             xtype = nc_file%variable(i)%attribute(j)%xtype, ndims = 1, i = k)
          end do
          write(un, '(2x,a)')&
          d//d//b//d//r//nc_value_print(value = nc_file%variable(i)%attribute(j)%value,&
          xtype = nc_file%variable(i)%attribute(j)%xtype, ndims = 1, i = k)
        ! for strings
        case(nf90_char)
          write(un, '(2x,a)', advance = 'no')&
          d//d//b//dr//trim(nc_file%variable(i)%attribute(j)%name)//' = "'
          ! attribute string values
          do k = 1, nc_file%variable(i)%attribute(j)%len
            write(un, '(a)', advance = 'no') nc_file%variable(i)%attribute(j)%value%char_1(k)
          end do
          write(un, '(a)') '"'
        end select
     end do
     select case(nc_file%variable(i)%attribute(j)%xtype)
     ! for numbers
     case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
       write(un, '(2x,a)', advance = 'no')&
       d//d//b//r//trim(nc_file%variable(i)%attribute(j)%name)//' = '
       ! attribute number values
       do k = 1, nc_file%variable(i)%attribute(j)%len
          write(un, '(a)', advance = 'no')&
          nc_value_print(value = nc_file%variable(i)%attribute(j)%value,&
          xtype = nc_file%variable(i)%attribute(j)%xtype, ndims = 1, i = k)//'; '
       end do
       write(un, *)
     ! for strings
     case(nf90_char)
       write(un, '(2x,a)', advance = 'no')&
       d//d//b//r//trim(nc_file%variable(i)%attribute(j)%name)//' = "'
       ! attribute string values
       do k = 1, nc_file%variable(i)%attribute(j)%len
         write(un, '(a)', advance = 'no') nc_file%variable(i)%attribute(j)%value%char_1(k)
       end do
       write(un, '(a)') '"'
     end select
  end do
  ! last variable
  write(un, '(2x,a)') d//r//trim(nc_file%variable(i)%name)//':'
  write(un, '(2x,a)') d//b//dr//'xtype: '//nc_xtype_info(nc_file%variable(i)%xtype)
  write(un, '(2x,a)') d//b//dr//'memory size: '//&
  number_to_string(real(nc_file%variable(i)%value%mem_bits, 4)/8/10**6,&
  frmt = '(f100.2)')//' MB'
  write(un, '(2x,a)', advance = 'no') d//b//dr//'dimid: '
  do j = 1, nc_file%variable(i)%ndims
    write(un, '(a)', advance = 'no') number_to_string(nc_file%variable(i)%dimids(j))//'; '
  end do
  write(un, *)
  write(un, '(2x,a)') d//b//r//'natts: '//number_to_string(nc_file%variable(i)%natts)
  ! attribute for last variable
  do j = 1, nc_file%variable(i)%natts - 1
     select case(nc_file%variable(i)%attribute(j)%xtype)
     ! for numbers
     case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
       write(un, '(2x,a)', advance = 'no')&
       d//b//b//dr//trim(nc_file%variable(i)%attribute(j)%name)//' = '
       do k = 1, nc_file%variable(i)%attribute(j)%len
          write(un, '(a)', advance = 'no') nc_value_print(nc_file%variable(i)%attribute(j)%value,&
          nc_file%variable(i)%attribute(j)%xtype, 1, k)//'; '
       end do
       write(un, *)
     ! for strings
     case(nf90_char)
       write(un, '(2x,a)', advance = 'no')&
       d//b//b//dr//trim(nc_file%variable(i)%attribute(j)%name)//' = "'
       do k = 1, nc_file%variable(i)%attribute(j)%len
         write(un, '(a)', advance = 'no') &
           nc_file%variable(i)%attribute(j)%value%char_1(k)
       end do
       write(un, '(a)') '"'
     end select
  end do
  select case(nc_file%variable(i)%attribute(j)%xtype)
  case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
    write(un, '(2x,a)', advance = 'no')&
    d//b//b//r//trim(nc_file%variable(i)%attribute(j)%name)//' = '
    do k = 1, nc_file%variable(i)%attribute(j)%len
       write(un, '(a)', advance = 'no') nc_value_print(nc_file%variable(i)%attribute(j)%value,&
       nc_file%variable(i)%attribute(j)%xtype, 1, k)//'; '
    end do
    write(un, *)
  case(nf90_char)
    write(un, '(2x,a)', advance = 'no')&
    d//b//b//r//trim(nc_file%variable(i)%attribute(j)%name)//' = "'
    do k = 1, nc_file%variable(i)%attribute(j)%len
      write(un, '(a)', advance = 'no') &
        nc_file%variable(i)%attribute(j)%value%char_1(k)
    end do
    write(un, '(a)') '"'
  end select

  write(un, '(2x,a)') dr//'nAttributes: '//number_to_string(nc_file%nattributes)
  do i = 1, nc_file%nattributes - 1
    select case(nc_file%attribute(i)%xtype)
    case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
      write(un, '(2x,a)') d//dr//trim(nc_file%attribute(i)%name)//' = '//&
        nc_value_print(nc_file%attribute(i)%value, nc_file%attribute(i)%xtype, 1, 1)
    case(nf90_char)
      write(un, '(2x,a)', advance = 'no') d//dr//trim(nc_file%attribute(i)%name)//' = "'
      do j = 1, nc_file%attribute(i)%len 
        write(un, '(a)', advance = 'no') nc_file%attribute(i)%value%char_1(j)
      end do
      write(un, '(a)') '"'
    end select
  end do
  select case(nc_file%attribute(i)%xtype)
  case(nf90_byte, nf90_short, nf90_int, nf90_int64, nf90_float, nf90_double, nf90_string)
    write(un, '(2x,a)') d//r//trim(nc_file%attribute(i)%name)//' = '//&
      nc_value_print(nc_file%attribute(i)%value, nc_file%attribute(i)%xtype, 1, 1)
  case(nf90_char)
    write(un, '(2x,a)', advance = 'no') d//r//trim(nc_file%attribute(i)%name)//' = "'
    do j = 1, nc_file%attribute(i)%len 
      write(un, '(a)', advance = 'no') nc_file%attribute(i)%value%char_1(j)
    end do
    write(un, '(a)') '"'
  end select

  write(un, '(2x,a)') dr//'unlimitedDimid: '//&
       number_to_string(nc_file%unlimiteddimid)
  write(un, '(2x,a)') r//'formatNum: '//&
       number_to_string(nc_file%formatnum)

 return

end subroutine nc_print_info
