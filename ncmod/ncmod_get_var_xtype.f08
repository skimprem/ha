subroutine get_var_xtype(ncid, varid, variable, verbose_phrase)
  use hamodule
  implicit none
  integer, intent(in) :: ncid, varid
  integer :: stdout, error_stat, i, j, k
  type(ncvariables), intent(inout) :: variable 
  character(*), intent(in), optional :: verbose_phrase
  logical :: verbose_mode
  stdout = 6
  error_stat = 0
  verbose_mode = .false.
  if(present(verbose_phrase) .eqv. .true.) then
    verbose_mode = .true.
  end if
  select case(variable%xtype)
  case(0) ! 0
    error_stat = 1
  case(nf90_byte) ! NC_BYTE: 8-bit signed integer
    select case(variable%ndims)
    case(1)
      allocate( variable%value_byte_1(variable%dimension(1)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_byte_1&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(8, 8)
    case(2)
      allocate( variable%value_byte_2(variable%dimension(1)%len, variable%dimension(2)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_byte_2&
             )&
            )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(variable%dimension(2)%len, 8) * int(8, 8)
    case(3)
      allocate(variable%value_byte_3(variable%dimension(1)%len, variable%dimension(2)%len, variable%dimension(3)%len))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_byte_3&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) *&
      int(variable%dimension(2)%len, 8) * int(variable%dimension(3)%len, 8) * int(8, 8)
    end select
  case(nf90_ubyte) ! NC_UBYTE: 8-bit unsigned integer
    error_stat = 1
  case(nf90_char) ! NC_CHAR: 8-bit character byte
    select case(variable%ndims)
    case(1)
      allocate( variable%value_char_1(variable%dimension(1)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_char_1&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(8, 8)
    case(2)
      allocate( variable%value_char_2(variable%dimension(1)%len, variable%dimension(2)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_char_2&
             )&
            )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(variable%dimension(2)%len, 8) * int(8, 8)
    case(3)
      allocate( variable%value_char_3(variable%dimension(1)%len, variable%dimension(2)%len, variable%dimension(3)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_char_3&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) *&
      int(variable%dimension(2)%len, 8) * int(variable%dimension(3)%len, 8) * int(8, 8)
    end select
  case(nf90_short) ! NC_SHORT: 16-bit signed integer
    select case(variable%ndims)
    case(1)
      allocate( variable%value_short_1(variable%dimension(1)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_short_1&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(16, 8)
    case(2)
      allocate( variable%value_short_2(variable%dimension(1)%len, variable%dimension(2)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_short_2&
             )&
            )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(variable%dimension(2)%len, 8) * int(16, 8)
    case(3)
      allocate( variable%value_short_3(variable%dimension(1)%len, variable%dimension(2)%len, variable%dimension(3)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_short_3&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) *&
      int(variable%dimension(2)%len, 8) * int(variable%dimension(3)%len, 8) * int(16, 8)
    end select
  case(nf90_ushort) ! NC_USHORT: 16-bit unsigned integer
    error_stat = 1
  case(nf90_int) ! NC_int: (NC_LONG): 32-bit signed integer
    select case(variable%ndims)
    case(1)
      allocate( variable%value_int_1(variable%dimension(1)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_int_1&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(32, 8)
    case(2)
      allocate( variable%value_int_2(variable%dimension(1)%len, variable%dimension(2)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_int_2&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(variable%dimension(2)%len, 8) * int(32, 8)
    case(3)
      allocate( variable%value_int_3(variable%dimension(1)%len, variable%dimension(2)%len, variable%dimension(3)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_int_3&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) *&
      int(variable%dimension(2)%len, 8) * int(variable%dimension(3)%len, 8) * int(32, 8)
    end select
  case(nf90_uint) ! NC_Uint: 32-bit unsigned integer
    error_stat = 1
  case(nf90_int64) ! NC_int64: 64-bit signed integer
    select case(variable%ndims)
    case(1)
      allocate( variable%value_int64_1(variable%dimension(1)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_int64_1&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(64, 8)
    case(2)
      allocate( variable%value_int64_2(variable%dimension(1)%len, variable%dimension(2)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_int64_2&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(variable%dimension(2)%len, 8) * int(64, 8)
    case(3)
      allocate( variable%value_int64_3(variable%dimension(1)%len, variable%dimension(2)%len, variable%dimension(3)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_int64_3&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) *&
      int(variable%dimension(2)%len, 8) * int(variable%dimension(3)%len, 8) * int(64, 8)
    end select
  case(nf90_uint64) ! NC_Uint64: 64-bit unsigned integer
    error_stat = 1
  case(nf90_float) ! NC_FLOAT: 32-bit floating point
    select case(variable%ndims)
    case(1)
      allocate( variable%value_float_1(variable%dimension(1)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_float_1&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(32, 8)
    case(2)
      allocate( variable%value_float_2(variable%dimension(1)%len, variable%dimension(2)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_float_2&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(variable%dimension(2)%len, 8) * int(32, 8)
    case(3)
      allocate( variable%value_float_3(variable%dimension(1)%len, variable%dimension(2)%len, variable%dimension(3)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_float_3&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) *&
      int(variable%dimension(2)%len, 8) * int(variable%dimension(3)%len, 8) * int(32)
    end select
  case(nf90_double) ! NC_double: 64-bit floating point
    select case(variable%ndims)
    case(1)
      allocate( variable%value_double_1(variable%dimension(1)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_double_1&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(64, 8)
    case(2)
      allocate( variable%value_double_2(variable%dimension(1)%len, variable%dimension(2)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_double_2&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) * int(variable%dimension(2)%len, 8) * int(64, 8)
    case(3)
      allocate( variable%value_double_3(variable%dimension(1)%len, variable%dimension(2)%len, variable%dimension(3)%len) )
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_double_3&
             )&
           )
      variable%mem_bits = int(variable%dimension(1)%len, 8) *&
      int(variable%dimension(2)%len, 8) * int(variable%dimension(3)%len, 8) * int(64, 8)
    end select
  case(nf90_string) ! NC_STRING: variable length character string
    variable%mem_bits = 0
    select case(variable%ndims)
    case(1)
      allocate(character(len=nf90_max_name) :: variable%value_string_1(variable%dimension(1)%len))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_string_1&
             )&
           )
    case(2)
      allocate(character(len=nf90_max_name) :: variable%value_string_2(variable%dimension(1)%len, variable%dimension(2)%len))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_string_2&
             )&
           )
    case(3)
      allocate(character(len=nf90_max_name) ::&
      variable%value_string_3(variable%dimension(1)%len, variable%dimension(2)%len, variable%dimension(3)%len))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = variable%value_string_3&
             )&
           )
    end select
  end select
  if(verbose_mode .eqv. .true.) then
    write(stdout, '(a)') verbose_phrase//'nc_get_var(): done!'
    write(stdout, '(a)') verbose_phrase//'  loaded values: '//ha_mem_info(variable%mem_bits, 'B')
  end if
  if(error_stat > 0) then
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'error: '//nc_xtype_info(variable%xtype)//' not set'
    end if
    stop
  end if
  return
end subroutine get_var_xtype
