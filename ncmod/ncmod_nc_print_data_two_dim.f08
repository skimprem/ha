subroutine nc_print_data_two_dim(nc_file, output_filename, verbose_phrase)
  use hamodule
  implicit none

  type string_array
    character(len=:), allocatable :: string
  end type
  type one_dim_char
    integer(kind=4) :: varid
    integer(kind=4) :: len
    type(string_array), dimension(:), allocatable :: value
  end type one_dim_char
  type two_dim_char
    integer(kind=4) :: varid
    integer(kind=4), dimension(2) :: len
    type(string_array), dimension(:,:), allocatable :: value
  end type two_dim_char

  type(ncfile), intent(in) :: nc_file
  type(one_dim_char), dimension(:), allocatable :: one_dim 
  type(two_dim_char), dimension(:), allocatable :: two_dim
  character(*), intent(in) :: output_filename
  character(len=:), allocatable :: string, frmt
  integer(kind=4) :: i, j, k, un, stdout, string_len 
  real(kind=8) :: progress_coef
  character(*), intent(in), optional :: verbose_phrase
  logical :: verbose_mode
  verbose_mode = .false.
  stdout = 6
  if((present(verbose_phrase) .eqv. .true.) .and. (trim(adjustl(verbose_phrase)) /= '')) then
    verbose_mode = .true.
  end if
  if(verbose_mode .eqv. .true.) then
    frmt = '(a, 1x, a)'
    write(stdout, frmt) verbose_phrase, 'begin nc_print_data()'
  end if
  open(newunit = un, file = output_filename)
  string = verbose_phrase//'   '//'Writing grid to file "'//output_filename//'"...: '
  string_len = len_trim(string)
  i = 0
  j = 0
  do k = 1, nc_file%nvariables
    select case(nc_file%variable(k)%ndims)
    case(0)
      cycle
    case(1)
      i = i + 1
    case(2)
      j = j + 1
    end select
  end do
  allocate(one_dim(i), two_dim(j))
  i = 0
  j = 0
  do k = 1, nc_file%nvariables
    select case(nc_file%variable(k)%ndims)
    case(0)
      cycle
    case(1)
      i = i + 1
      one_dim(i)%varid = k
      one_dim(i)%len = nc_file%dimension(nc_file%variable(one_dim(i)%varid)%dimids(1))%len
    case(2)
      j = j + 1
      two_dim(j)%varid = k
      two_dim(j)%len(1) = nc_file%dimension(nc_file%variable(two_dim(j)%varid)%dimids(1))%len
      two_dim(j)%len(2) = nc_file%dimension(nc_file%variable(two_dim(j)%varid)%dimids(2))%len
    end select
  end do
  if(verbose_mode .eqv. .true.) then
    frmt = '(a, 2x, 4a)'
    write(stdout, frmt)&
    (verbose_phrase, 'dimension ', number_to_string(k)//': ', nc_file%variable(one_dim(k)%varid)%name,&
    '('//number_to_string(one_dim(k)%len)//')', k = 1, size(one_dim))
  end if
  do k = 1, size(one_dim)
    allocate( one_dim(k)%value(one_dim(k)%len) )
    if(verbose_mode .eqv. .true.) then
      frmt = '(a, 2x, a)'
      write(stdout, frmt) verbose_phrase, 'allocated one dimension string array: done!'
    end if
    select case(nc_file%variable(one_dim(k)%varid)%xtype)
    case(nf90_byte)
      do j = 1, one_dim(k)%len
        one_dim(k)%value(j)%string = number_to_string(nc_file%variable(one_dim(k)%varid)%value_byte_1(j))
      end do
    case(nf90_short)
      do j = 1, one_dim(k)%len
        one_dim(k)%value(j)%string = number_to_string(nc_file%variable(one_dim(k)%varid)%value_short_1(j))
      end do
    case(nf90_int)
      do j = 1, one_dim(k)%len
        one_dim(k)%value(j)%string = number_to_string(nc_file%variable(one_dim(k)%varid)%value_int_1(j))
      end do
    case(nf90_int64)
      do j = 1, one_dim(k)%len
        one_dim(k)%value(j)%string = number_to_string(nc_file%variable(one_dim(k)%varid)%value_int64_1(j))
      end do
    case(nf90_float)
      do j = 1, one_dim(k)%len
        one_dim(k)%value(j)%string = number_to_string(nc_file%variable(one_dim(k)%varid)%value_float_1(j))
      end do
    case(nf90_double)
      do j = 1, one_dim(k)%len
        one_dim(k)%value(j)%string = number_to_string(nc_file%variable(one_dim(k)%varid)%value_double_1(j))
      end do
    case(nf90_char)
      do j = 1, one_dim(k)%len
        one_dim(k)%value(j)%string = nc_file%variable(one_dim(k)%varid)%value_char_1(j)
      end do
    case(nf90_string)
      do j = 1, one_dim(k)%len
        one_dim(k)%value(j)%string = nc_file%variable(one_dim(k)%varid)%value_string_1(j)
      end do
    end select
    if(verbose_mode .eqv. .true.) then
      frmt = '(a, 2x, a)'
      write(stdout, frmt) verbose_phrase, 'loaded values to string array: done!'
    end if
  end do

  do k = 1, size(two_dim)
    allocate( two_dim(k)%value(two_dim(k)%len(1), two_dim(k)%len(2)) )
    if(verbose_mode .eqv. .true.) then
      frmt = '(a, 2x, a)'
      write(stdout, frmt) verbose_phrase, 'allocated two dimension string array: done!'
    end if
    select case(nc_file%variable(two_dim(k)%varid)%xtype)
    case(nf90_byte)
      do i = 1, two_dim(k)%len(1)
        do j = 1, two_dim(k)%len(2)
          two_dim(k)%value(i, j)%string = number_to_string(nc_file%variable(two_dim(k)%varid)%value_byte_2(i, j))
        end do
      end do
    case(nf90_short)
      do i = 1, two_dim(k)%len(1)
        do j = 1, two_dim(k)%len(2)
          two_dim(k)%value(i, j)%string = number_to_string(nc_file%variable(two_dim(k)%varid)%value_short_2(i, j))
        end do
      end do
    case(nf90_int)
      do i = 1, two_dim(k)%len(1)
        do j = 1, two_dim(k)%len(2)
          two_dim(k)%value(i, j)%string = number_to_string(nc_file%variable(two_dim(k)%varid)%value_int_2(i, j))
        end do
      end do
    case(nf90_int64)
      do i = 1, two_dim(k)%len(1)
        do j = 1, two_dim(k)%len(2)
          two_dim(k)%value(i, j)%string = number_to_string(nc_file%variable(two_dim(k)%varid)%value_int64_2(i, j))
        end do
      end do
    case(nf90_float)
      do i = 1, two_dim(k)%len(1)
        do j = 1, two_dim(k)%len(2)
          two_dim(k)%value(i, j)%string = number_to_string(nc_file%variable(two_dim(k)%varid)%value_float_2(i, j))
        end do
      end do
    case(nf90_double)
      do i = 1, two_dim(k)%len(1)
        do j = 1, two_dim(k)%len(2)
          two_dim(k)%value(i, j)%string = number_to_string(nc_file%variable(two_dim(k)%varid)%value_double_2(i, j))
        end do
      end do
    case(nf90_char)
      do i = 1, two_dim(k)%len(1)
        do j = 1, two_dim(k)%len(2)
          two_dim(k)%value(i, j)%string = nc_file%variable(two_dim(k)%varid)%value_char_2(i, j)
        end do
      end do
    case(nf90_string)
      do i = 1, two_dim(k)%len(1)
        do j = 1, two_dim(k)%len(2)
          two_dim(k)%value(i, j)%string = nc_file%variable(two_dim(k)%varid)%value_string_2(i, j)
        end do
      end do
    end select
    if(verbose_mode .eqv. .true.) then
      frmt = '(a, 2x, a)'
      write(stdout, frmt) verbose_phrase, 'loaded values to string array: done!'
    end if
  end do

  if(size(one_dim) == 2) then
    if(verbose_mode .eqv. .true.) then
      frmt = '(a, 2x a)'
      write(stdout, frmt) verbose_phrase, 'writing grid to file...'
    end if
    write(un, '(a)', advance = 'no')&
    nc_file%variable(one_dim(1)%varid)%name//' '//nc_file%variable(one_dim(2)%varid)%name
    write(un, '(a)', advance = 'no') (' '//nc_file%variable(two_dim(k)%varid)%name, k = 1, size(two_dim))
    write(un, *)
    do i = 1, one_dim(1)%len
      do j = 1, one_dim(2)%len
        write(un, '(a)', advance = 'no') one_dim(1)%value(i)%string//' '//one_dim(2)%value(j)%string//' '//&
        two_dim(1)%value(j, i)%string
        !write(un, '(a)', advance = 'no') (' '//two_dim(k)%value(i, j)%string, k = 1, size(two_dim))
        write(un, *)
      end do
    end do
  end if

  !progress_coef = 100.0 / real(nc_file%variable(k)%dimension(1)%len, 4)
  !write(un, '(a)')&
  !trim(adjustl(nc_file%variable(1)%name))//' '//&
  !trim(adjustl(nc_file%variable(2)%name))//' '//&
  !trim(adjustl(nc_file%variable(3)%name))
  !do i = 1, nc_file%dimension(1)%len 
    !do j = 1,  nc_file%dimension(2)%len
      !write(un, '(a)')&
      !trim_null(nc_value_print(value = nc_file%variable(1)%value,&
                     !xtype = nc_file%variable(1)%xtype,&
                     !ndims = nc_file%variable(1)%ndims,&
                     !i = i))//' '//&
      !trim_null(nc_value_print(value = nc_file%variable(2)%value,&
                     !xtype = nc_file%variable(2)%xtype,&
                     !ndims = nc_file%variable(2)%ndims,&
                     !i = j))//' '//&
      !trim_null(nc_value_print(value = nc_file%variable(3)%value,&
                     !xtype = nc_file%variable(3)%xtype,&
                     !ndims = nc_file%variable(3)%ndims,&
                     !i = i, j = j))
    !end do
    !do j =  1, string_len + 17 
      !write(stdout, '(a)', advance = 'no') '\b'
    !end do
    !write(stdout, '(a)', advance = 'no') string
    !write(stdout, '(f5.1)', advance = 'no') real(i, 4) * progress_coef
    !write(stdout, '(a)', advance =  'no') '% completed'
  !end do
  !write(stdout, '(a)')
  if(verbose_mode .eqv. .true.) then
    frmt = '(a, 1x, a)'
    write(stdout, frmt) verbose_phrase, 'end nc_print_data()'
  end if
  return
end subroutine nc_print_data_two_dim
