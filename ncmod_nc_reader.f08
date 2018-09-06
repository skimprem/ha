subroutine nc_reader(nc_file, verbose)

  use netcdf
  use hamodule

  implicit none
  type(ncfile), intent(inout) :: nc_file
  integer :: k = 0, i, j, ncstatus, stdout
  character(*), intent(in), optional :: verbose
  logical :: verbose_mode

  stdout = 6

  if(present(verbose) .eqv. .true. .and. trim(adjustl(verbose)) /= '') then
    verbose_mode = .true.
  else if(present(verbose) .eqv. .true. .and. trim(adjustl(verbose)) == '') then
    verbose_mode = .false.
  else if(present(verbose) .eqv. .false.) then
    verbose_mode = .false.
  end if

  if(verbose_mode .eqv. .true.)&
  write(stdout, '(a)') verbose//' begin nc_reader(): '//trim(adjustl(nc_file%path))

  nc_file%cmode = nf90_nowrite

  call nc_error_check(&
       'nc_open',&
       nf90_open(&
         path = nc_file%path,&
         mode = nc_file%cmode,&
         ncid = nc_file%ncid&
         )&
       )

  if(verbose_mode .eqv. .true.)&
  write(stdout, '(a)') verbose//'   nc_open: '//trim(adjustl(nc_file%path))

  call nc_error_check(&
       'nc_inquire',&
       nf90_inquire(&
         ncid = nc_file%ncid,&
         ndimensions = nc_file%ndimensions,&
         nvariables = nc_file%nvariables,&
         nattributes = nc_file%nattributes,&
         unlimiteddimid = nc_file%unlimiteddimid,&
         formatnum = nc_file%formatnum&
         )&
       )

  if(verbose_mode .eqv. .true.)&
  write(stdout, '(a)') verbose//'   nc_inquire: '

  allocate(&
       nc_file%dimension(nc_file%ndimensions),&
       nc_file%variable(nc_file%nvariables),&
       nc_file%attribute(nc_file%nattributes)&
       )

  if(verbose_mode .eqv. .true.)&
  write(stdout, '(a)') verbose//'   allocated: '//number_to_string(nc_file%ndimensions)//&
  ' dimensions, '//number_to_string(nc_file%nvariables)//' variables, '//&
  number_to_string(nc_file%nattributes)//' attributes'

  do i = 1, nc_file%nattributes

    nc_file%attribute(i)%attnum = i

    call nc_error_check(&
        'nc_inq_attname',&
        nf90_inq_attname(&
          ncid = nc_file%ncid,&
          varid = nf90_global,&
          attnum = nc_file%attribute(i)%attnum,&
          name = nc_file%attribute(i)%name&
          )&
        )

    if(verbose_mode .eqv. .true.)&
    write(stdout, '(a)') verbose//'   nc_inq_attname: '//trim(adjustl(nc_file%attribute(i)%name))

    call nc_error_check(&
         'nc_inquire_attribute',&
         nf90_inquire_attribute(&
           ncid = nc_file%ncid,&
           varid = nf90_global,&
           name = nc_file%attribute(i)%name,&
           xtype = nc_file%attribute(i)%xtype,&
           len = nc_file%attribute(i)%len&
           )&
         )

    if(verbose_mode .eqv. .true.)&
    write(stdout, '(a)') verbose//'   inquire_attribute: '
    
    if(verbose_mode .eqv. .true.) then
      call get_att_xtype(&
           ncid = nc_file%ncid,&
           varid = nf90_global,&
           xtype = nc_file%attribute(i)%xtype,&
           name = nc_file%attribute(i)%name,&
           len = nc_file%attribute(i)%len,&
           value = nc_file%attribute(i)%value,&
           verbose = verbose//'  ')
    else
      call get_att_xtype(&
           ncid = nc_file%ncid,&
           varid = nf90_global,&
           xtype = nc_file%attribute(i)%xtype,&
           name = nc_file%attribute(i)%name,&
           len = nc_file%attribute(i)%len,&
           value = nc_file%attribute(i)%value,&
    end if

  end do

  do i = 1, nc_file%ndimensions

     nc_file%dimension(i)%dimid = i

     call nc_error_check(&
          'nc_inquire_dimension',&
          nf90_inquire_dimension(&
            ncid = nc_file%ncid,&
            dimid = nc_file%dimension(i)%dimid,&
            name = nc_file%dimension(i)%name,&
            len = nc_file%dimension(i)%len&
            )&
          )
  end do

  do i = 1, nc_file%nvariables

     nc_file%variable(i)%varid = i

     call nc_error_check(&
          'nc_inquire_variable',&
          nf90_inquire_variable(&
            ncid = nc_file%ncid,&
            varid = nc_file%variable(i)%varid,&
            name = nc_file%variable(i)%name,&
            xtype = nc_file%variable(i)%xtype,&
            ndims = nc_file%variable(i)%ndims,&
            natts = nc_file%variable(i)%natts&
            )&
          )

     allocate(&
          nc_file%variable(i)%dimids(nc_file%variable(i)%ndims),&
          nc_file%variable(i)%attribute(nc_file%variable(i)%natts)&
          )

     call nc_error_check(&
          'nc_inquire_variable',&
          nf90_inquire_variable(&
            ncid = nc_file%ncid,&
            varid = nc_file%variable(i)%varid,&
            dimids = nc_file%variable(i)%dimids&
            )&
          )

     do j = 1, nc_file%variable(i)%natts

        nc_file%variable(i)%attribute(j)%attnum = j

        call nc_error_check(&
             'nc_inq_attname',&
             nf90_inq_attname(&
               ncid = nc_file%ncid,&
               varid = nc_file%variable(i)%varid,&
               attnum = nc_file%variable(i)%attribute(j)%attnum,&
               name = nc_file%variable(i)%attribute(j)%name&
               )&
             )

        call nc_error_check(&
             'nc_inquire_attribute',&
             nf90_inquire_attribute(&
               ncid = nc_file%ncid,&
               varid = nc_file%variable(i)%varid,&
               name = nc_file%variable(i)%attribute(j)%name,&
               xtype = nc_file%variable(i)%attribute(j)%xtype,&
               len = nc_file%variable(i)%attribute(j)%len&
               )&
             )

      if(verbose_mode .eqv. .true.)
        call get_att_xtype(&
             nc_file%ncid&
             ,nc_file%variable(i)%varid&
             ,nc_file%variable(i)%attribute(j)%xtype&
             ,nc_file%variable(i)%attribute(j)%name&
             ,nc_file%variable(i)%attribute(j)%len&
             ,nc_file%variable(i)%attribute(j)%value&
             ,verbose = verbose//'  '&
             )
      else
        call get_att_xtype(&
             nc_file%ncid&
             ,nc_file%variable(i)%varid&
             ,nc_file%variable(i)%attribute(j)%xtype&
             ,nc_file%variable(i)%attribute(j)%name&
             ,nc_file%variable(i)%attribute(j)%len&
             ,nc_file%variable(i)%attribute(j)%value&
             )
      end if

     end do

     allocate( nc_file%variable(i)%len(nc_file%variable(i)%ndims) )

     do j = 1, nc_file%variable(i)%ndims
        call nc_error_check(&
             'nc_inquire_dimension',&
             nf90_inquire_dimension(&
               ncid = nc_file%ncid,&
               dimid = nc_file%variable(i)%dimids(j),&
               len = nc_file%variable(i)%len(j)&
               )&
             )
     end do

    if(verbose_mode .eqv. .true.) then
       call get_var_xtype(&
           ncid = nc_file%ncid,&
           varid = nc_file%variable(i)%varid,&
           xtype = nc_file%variable(i)%xtype,&
           ndims = nc_file%variable(i)%ndims,&
           len = nc_file%variable(i)%len,&
           value = nc_file%variable(i)%value,&
           verbose = verbose//'  '&
           )
    else
       call get_var_xtype(&
           ncid = nc_file%ncid,&
           varid = nc_file%variable(i)%varid,&
           xtype = nc_file%variable(i)%xtype,&
           ndims = nc_file%variable(i)%ndims,&
           len = nc_file%variable(i)%len,&
           value = nc_file%variable(i)%value,&
           )
    end if

  end do

  if(verbose_mode .eqv. .true.) write(stdout, '(a)') verbose//' end nc_reader()'

  return

end subroutine nc_reader
