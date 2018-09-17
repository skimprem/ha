subroutine nc_reader(nc_file, verbose_phrase)
  use netcdf
  use hamodule
  implicit none
  type(ncfile), intent(inout) :: nc_file
  integer :: k = 0, i, j, ncstatus, stdout
  character(*), intent(in), optional :: verbose_phrase
  logical :: verbose_mode
  stdout = 6
  verbose_mode = .false.
  if((present(verbose_phrase) .eqv. .true.) .and. (trim(adjustl(verbose_phrase)) /= '')) then
    verbose_mode = .true.
  end if
  if(verbose_mode .eqv. .true.) then
    write(stdout, '(a)') verbose_phrase//' begin nc_reader(): '//nc_file%path
  end if
! ###########################################################################################
! 1. READ HEADER OF FILE
! ###########################################################################################
  nc_file%cmode = nf90_nowrite
  call nc_error_check(&
       'nc_open',&
       nf90_open(&
         path = nc_file%path,&          ! intent(in)
         mode = nc_file%mode,&          ! intent(in)
         ncid = nc_file%ncid,&          ! intent(out)
         chunksize = nc_file%chunksize& ! intent(inout)
         )&
       )
  if(verbose_mode .eqv. .true.) then
    write(stdout, '(a)') verbose_phrase//'   nc_open(): '
    write(stdout, '(a)') verbose_phrase//'     path: '//nc_file%path
    write(stdout, '(a)') verbose_phrase//'     cmode: '//number_to_string(nc_file%cmode)
    write(stdout, '(a)') verbose_phrase//'     ncid: '//number_to_string(nc_file%ncid)
    write(stdout, '(a)') verbose_phrase//'     chunksize: '//number_to_string(nc_file%chunksize)
  end if
  call nc_error_check(&
       'nc_inquire',&
       nf90_inquire(&
         ncid = nc_file%ncid,&                     ! intent(in)
         ndimensions = nc_file%ndimensions,&       ! intent(out)
         nvariables = nc_file%nvariables,&         ! intent(out)
         nattributes = nc_file%nattributes,&       ! intent(out)
         unlimiteddimid = nc_file%unlimiteddimid,& ! intent(out)
         formatnum = nc_file%formatnum&            ! intent(out)
         )&
       )
  if(verbose_mode .eqv. .true.) then
    write(stdout, '(a)') verbose_phrase//'   nc_inquire(): '
    write(stdout, '(a)') verbose_phrase//'     ndimensions: '//number_to_string(nc_file%ndimensions)
    write(stdout, '(a)') verbose_phrase//'     nvariables: '//number_to_string(nc_file%nvariables)
    write(stdout, '(a)') verbose_phrase//'     nattributes: '//number_to_string(nc_file%nattributes)
    write(stdout, '(a)') verbose_phrase//'     nunlimiteddimid: '&
                                                        //number_to_string(nc_file%nunlimiteddimid)
  end if
  allocate(&
       nc_file%dimension(nc_file%ndimensions),&
       nc_file%variable(nc_file%nvariables),&
       nc_file%attribute(nc_file%nattributes)&
       )
  if(verbose_mode .eqv. .true.) then
    write(stdout, '(a)') verbose_phrase//'   allocated: '//number_to_string(nc_file%ndimensions)//&
    ' dimensions, '//number_to_string(nc_file%nvariables)//' variables, '//&
    number_to_string(nc_file%nattributes)//' attributes'
  end if
! ###########################################################################################
! 2. READ GLOBAL ATTRIBUTES
! ###########################################################################################
  do i = 1, nc_file%nattributes
    nc_file%attribute(i)%attnum = i
    call nc_error_check(&
        'nc_inq_attname',&
        nf90_inq_attname(&
          ncid = nc_file%ncid,&                  ! intent(in)
          varid = nf90_global,&                  ! intent(in)
          attnum = nc_file%attribute(i)%attnum,& ! intent(in)
          name = nc_file%attribute(i)%name&      ! intent(out)
          )&
        )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)')&
      verbose_phrase//'   nc_inq_attname(): '//trim(adjustl(nc_file%attribute(i)%name))
    end if
    call nc_error_check(&
         'nc_inquire_attribute',&
         nf90_inquire_attribute(&
           ncid = nc_file%ncid,&                ! intent(in)
           varid = nf90_global,&                ! intent(in)
           name = nc_file%attribute(i)%name,&   ! intent(in)
           xtype = nc_file%attribute(i)%xtype,& ! intent(out)
           len = nc_file%attribute(i)%len&      ! intent(out)
           )&
         )
    if(verbose_mode .eqv. .true.) then
      write(stdout, '(a)') verbose_phrase//'   nc_inquire_attribute(): '
      write(stdout, '(a)') verbose_phrase//'     xtype: '&
                                                  //number_to_string(nc_file%attribute(i)%xtype)
      write(stdout, '(a)') verbose_phrase//'     len: '//number_to_string(nc_file%attribute(i)%len)
    end if
    if(verbose_mode .eqv. .true.) then
      call get_att_xtype(&
           ncid = nc_file%ncid,&                  ! intent(in)
           varid = nf90_global,&                  ! intent(in)
           attribute = nc_file%attribute(i),&     ! intent(inout)
           verbose_phrase = verbose_phrase//'  ') ! intent(in)
    else
      call get_att_xtype(& ! intent
           ncid = nc_file%ncid,& ! intent
           varid = nf90_global,& ! intent
           attribute = nc_file%attribute(i)& ! intent
           )
    end if
  end do
! ###########################################################################################
! READ DIMENSIONS
! ###########################################################################################
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
! ###########################################################################################
! 3. READ VARIABLES
! ###########################################################################################
  do i = 1, nc_file%nvariables
    nc_file%variable(i)%varid = i
    call nc_error_check(&
      'nc_inquire_variable',&
      nf90_inquire_variable(&
        ncid = nc_file%ncid,&               ! intent(in)
        varid = nc_file%variable(i)%varid,& ! intent(in)
        name = nc_file%variable(i)%name,&   ! intent(out)
        xtype = nc_file%variable(i)%xtype,& ! intent(out)
        ndims = nc_file%variable(i)%ndims,& ! intent(out)
        natts = nc_file%variable(i)%natts&  ! intent(out)
        )&
      )
    allocate(nc_file%variable(i)%dimids(nc_file%variable(i)%ndims))
    call nc_error_check(&
      'nc_inquire_variable',&
      nf90_inquire_variable(&
        ncid = nc_file%ncid,&
        varid = nc_file%variable(i)%varid,&
        dimids = nc_file%variable(i)%dimids&
        )&
      )
! *******************************************************************************************
! 3.1 READ VARIABLE ATTRIBUTES
! *******************************************************************************************
    allocate(nc_file%variable(i)%attribute(nc_file%variable(i)%natts))
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
      if(verbose_mode .eqv. .true.) then
        call get_att_xtype(&
             ncid = nc_file%ncid, &
             varid = nc_file%variable(i)%varid,&
             attribute = nc_file%variable(i)%attribute(j),&
             verbose_phrase = verbose_phrase//'  '&
             )
      else
        call get_att_xtype(&
             ncid = nc_file%ncid,&
             varid = nc_file%variable(i)%varid,&
             attribute = nc_file%variable(i)%attribute(j)&
             )
      end if
    end do
! *******************************************************************************************
! 3.2 
! *******************************************************************************************
    !do j = 1, nc_file%variable(i)%ndims
      !call nc_error_check(&
        !'nc_inquire_dimension',&
        !nf90_inquire_dimension(&
          !ncid = nc_file%ncid,&
          !dimid = nc_file%variable(i)%dimids(j),&
          !len = nc_file%variable(i)%dimension(nc_file%variable(i)%dimids(j))%len&
          !!)&
        !)
    !end do
    !allocate(&
      !nc_file%variable(i)%dimension(nc_file%variable(i)%ndims),&
      !)

    !if(verbose_mode .eqv. .true.) then
      !call get_var_xtype(&
        !ncid = nc_file%ncid,&
        !varid = nc_file%variable(i)%varid,&
        !xtype = nc_file%variable(i)%xtype,&
        !ndims = nc_file%variable(i)%ndims,&
        !len = nc_file%variable(i)%len,&
        !value = nc_file%variable(i)%value,&
        !verbose_phrase = verbose_phrase//'  '&
        !)
    !else
      !call get_var_xtype(&
        !ncid = nc_file%ncid,&
        !varid = nc_file%variable(i)%varid,&
        !xtype = nc_file%variable(i)%xtype,&
        !ndims = nc_file%variable(i)%ndims,&
        !len = nc_file%variable(i)%len,&
        !value = nc_file%variable(i)%value&
        !)
    !end if
  end do
  if(verbose_mode .eqv. .true.) then
    write(stdout, '(a)') verbose_phrase//' end nc_reader()'
  end if
  return
end subroutine nc_reader
