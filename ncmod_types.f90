  type ncdimensions
    character(nf90_max_name) ::&
        name
    integer ::&
        dimid,&
        len
  end type ncdimensions

  type ncvalues

    integer(kind=1), allocatable :: byte_1(:), byte_2(:,:), byte_3(:,:,:)
    integer(kind=2), allocatable :: short_1(:), short_2(:,:), short_3(:,:,:)
    integer(kind=4), allocatable :: int_1(:), int_2(:,:), int_3(:,:,:)
    integer(kind=8), allocatable :: int64_1(:), int64_2(:,:), int64_3(:,:,:)
    real(kind=4), allocatable :: float_1(:), float_2(:,:), float_3(:,:,:)
    real(kind=8), allocatable :: double_1(:), double_2(:,:), double_3(:,:,:)
    character(kind=1, len=1), allocatable ::&
      char_1(:), char_2(:,:), char_3(:,:,:)
    character(kind=1, len=nf90_max_name), allocatable ::&
      string_1(:), string_2(:,:), string_3(:,:,:)
    integer(kind=8) :: mem_bits

  end type ncvalues

  type ncattributes
    character(nf90_max_name) ::&
        name
    integer ::&
        xtype,&
        len,&
        attnum 
    type(ncvalues) ::&
        value
  end type ncattributes

  type ncxtypes
    character(nf90_max_name) ::&
        name
    integer ::&
        type,&
        size
  end type ncxtypes

  type ncvariables
    character(nf90_max_name) ::&
        name
    integer ::&
        varid,&
        xtype,&
        ndims,&
        natts
    integer, dimension(:), allocatable ::&
        dimids
    integer, dimension(:), allocatable ::&
        len
    type(ncattributes), dimension(:), allocatable :: attribute
    type(ncvalues) :: value

  end type ncvariables

  type ncfile
    character(len=:), allocatable :: path
    character(len=:), allocatable ::& !
        name,&
        title,&
        history,&
        conventions
    integer ::& !
        cmode,& !
        ncid,& !
        ndimensions,& !
        nvariables,& !
        nattributes,& !
        unlimiteddimid,& !
        formatnum !
    type(ncdimensions), dimension(:), allocatable :: dimension
    type(ncvariables), dimension(:), allocatable :: variable
    type(ncattributes), dimension(:), allocatable :: attribute
  end type ncfile


