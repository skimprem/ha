subroutine get_var_xtype(&
     ncid,& !
     varid,& !
     xtype,& !
     ndims,& !
     len,&
     value,&
     verbose) !

  use hamodule

  implicit none
  integer, intent(in) :: ncid, varid, xtype, ndims
  integer, intent(in), dimension(ndims) :: len
  integer :: stdout = 6, i, j, k
  type(ncvalues), intent(out) :: value
  character(*), intent(in), optional :: verbose
  logical :: verbose_mode

  verbose_mode = .false.

  if(present(verbose) .eqv. .true.) verbose_mode = .true.

  if(verbose_mode .eqv. .true.)&
  write(stdout, '(a)') verbose//' begin get_var_xtype(): varid = '//number_to_string(varid)

  select case(xtype)
  case(0)
    ! 0
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   error: '//nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_byte)
    ! NC_BYTE: 8-bit signed integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   xtype: '//nc_xtype_info(xtype)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   dimension: kind = '//&
    number_to_string(ndims)
    select case(ndims)
    case(1)
      allocate( value%byte_1(len(1)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%byte_1&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(8, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(2)
      allocate( value%byte_2(len(1), len(2)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%byte_2&
             )&
            )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(8, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(3)
      allocate( value%byte_3(len(1), len(2), len(3)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))//';'//number_to_string(len(3))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%byte_3&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(len(3), 8) * int(8, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    end select
  case(nf90_ubyte)
    ! NC_UBYTE: 8-bit unsigned integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   error: '//nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_char)
    ! NC_CHAR: 8-bit character byte
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   xtype: '//nc_xtype_info(xtype)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   dimension: kind = '//&
    number_to_string(ndims)
    select case(ndims)
    case(1)
      allocate( value%char_1(len(1)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%char_1&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(8, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(2)
      allocate( value%char_2(len(1), len(2)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%char_2&
             )&
            )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(8, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(3)
      allocate( value%char_3(len(1), len(2), len(3)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%char_3&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(len(3), 8) * int(8, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    end select
  case(nf90_short)
    ! NC_SHORT: 16-bit signed integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   xtype: '//nc_xtype_info(xtype)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   dimension: kind = '//&
    number_to_string(ndims)
    select case(ndims)
    case(1)
      allocate( value%short_1(len(1)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%short_1&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(16, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(2)
      allocate( value%short_2(len(1), len(2)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%short_2&
             )&
            )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(16, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(3)
      allocate( value%short_3(len(1), len(2), len(3)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%short_3&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(len(3), 8) * int(16, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    end select
  case(nf90_ushort)
    ! NC_USHORT: 16-bit unsigned integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   error: '//nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_int)
    ! NC_int: (NC_LONG): 32-bit signed integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   xtype: '//nc_xtype_info(xtype)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   dimension: kind = '//&
    number_to_string(ndims)
    select case(ndims)
    case(1)
      allocate( value%int_1(len(1)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%int_1&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(32, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(2)
      allocate( value%int_2(len(1), len(2)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%int_2&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(32, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(3)
      allocate( value%int_3(len(1), len(2), len(3)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%int_3&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(len(3), 8) * int(32, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    end select
  case(nf90_uint)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   error: '//nc_xtype_info(xtype)//' not set'
    stop
    ! NC_Uint: 32-bit unsigned integer
    write(stdout, '(a)') nc_xtype_info(xtype)//' not set'
    stop 
  case(nf90_int64)
    ! NC_int64: 64-bit signed integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   xtype: '//nc_xtype_info(xtype)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   dimension: kind = '//&
    number_to_string(ndims)
    select case(ndims)
    case(1)
      allocate( value%int64_1(len(1)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%int64_1&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(64, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(2)
      allocate( value%int64_2(len(1), len(2)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%int64_2&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(64, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(3)
      allocate( value%int64_3(len(1), len(2), len(3)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%int64_3&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(len(3), 8) * int(64, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    end select
  case(nf90_uint64)
    ! NC_Uint64: 64-bit unsigned integer
    if(verbose_mode .eqv. .true.) write(stdout, '(a)')&
    verbose//'   error: '//nc_xtype_info(xtype)//' not set'
    stop
  case(nf90_float)
    ! NC_FLOAT: 32-bit floating point
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   xtype: '//nc_xtype_info(xtype)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   dimension: kind = '//&
    number_to_string(ndims)
    select case(ndims)
    case(1)
      allocate( value%float_1(len(1)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%float_1&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(32, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(2)
      allocate( value%float_2(len(1), len(2)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%float_2&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(32, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(3)
      allocate( value%float_3(len(1), len(2), len(3)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%float_3&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(len(3), 8) * int(32)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    end select
  case(nf90_double)
    ! NC_double: 64-bit floating point
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   xtype: '//nc_xtype_info(xtype)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   dimension: kind = '//&
    number_to_string(ndims)
    select case(ndims)
    case(1)
      allocate( value%double_1(len(1)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%double_1&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(64, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(2)
      allocate( value%double_2(len(1), len(2)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%double_2&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(64, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    case(3)
      allocate( value%double_3(len(1), len(2), len(3)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
      call nc_error_check(&
           'nc_get_var',&
           nf90_get_var(&
             ncid = ncid,&
             varid = varid,&
             values = value%double_3&
             )&
           )
      value%mem_bits = int(len(1), 8) * int(len(2), 8) * int(len(3), 8) * int(64, 8)
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   values: loaded'
    end select
  case(nf90_string)
    ! NC_STRING: variable length character string
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   xtype: '//nc_xtype_info(xtype)
    if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   dimension: kind = '//&
    number_to_string(ndims)
    value%mem_bits = 0
    select case(ndims)
    case(1)
      allocate( value%string_1(len(1)) )
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))
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
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
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
      if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//'   allocated: len = '//&
      number_to_string(len(1))//';'//number_to_string(len(2))
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

  if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//' end get_var_xtype()'

  return

end subroutine get_var_xtype
