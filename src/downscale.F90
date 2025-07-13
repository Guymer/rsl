PROGRAM main
    ! Import standard modules ...
    USE ISO_FORTRAN_ENV

    ! Import my modules ...
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_BIN,                      &
                                    sub_shrink_array

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 13200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 24600_INT64

    ! Declare variables ...
    CHARACTER(len = 256)                                                        :: fName
    INTEGER(kind = INT64)                                                       :: shrinkScale
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: elev
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: shrunkenElev

    ! **************************************************************************

    ! Allocate (1.21 GiB) array and populate it ...
    CALL sub_allocate_array(                                                    &
        elev,                                                                   &
        "elev",                                                                 &
        nx,                                                                     &
        ny,                                                                     &
        .TRUE._INT8                                                             &
    )
    CALL sub_load_array_from_BIN(                                               &
        elev,                                                                   &
        "../terr50_gagg_gb.bin"                                                 &
    )                                                                           ! [m]

    ! **************************************************************************

    ! Loop over possible shrink scales ...
    DO shrinkScale = 2_INT64, MIN(nx, ny) / 2_INT64, 1_INT64
        ! Skip this shrink scale if it is not an integer division of both axes
        ! of the array ...
        IF(MODULO(nx, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF
        IF(MODULO(ny, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF

        ! Create file name ...
        WRITE(fName, '("../terr50_gagg_gb_", i3.3, "x.bin")') shrinkScale

        ! Shrink array, save it and clean up ...
        CALL sub_shrink_array(                                                  &
                     nx = nx,                                                   &
                     ny = ny,                                                   &
                    arr = elev,                                                 &
            shrinkScale = shrinkScale,                                          &
            shrunkenArr = shrunkenElev                                          &
        )                                                                       ! [m]
        CALL sub_save_array_as_BIN(                                             &
            shrunkenElev,                                                       &
            TRIM(fName)                                                         &
        )
        DEALLOCATE(shrunkenElev)
    END DO

    ! **************************************************************************

    ! Clean up ...
    DEALLOCATE(elev)
END PROGRAM main
