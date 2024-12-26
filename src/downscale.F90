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
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: elev
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: shrunkenElev

    ! **************************************************************************

    ! Allocate (1.21 GiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx, ny, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../terr50_gagg_gb.bin")                 ! [m]

    ! **************************************************************************

    ! Shrink by 2x array and save it ...
    CALL sub_shrink_array(                                                      &
                 nx = nx,                                                       &
                 ny = ny,                                                       &
                arr = elev,                                                     &
        shrinkScale = 2_INT64,                                                  &
        shrunkenArr = shrunkenElev                                              &
    )
    CALL sub_save_array_as_BIN(shrunkenElev, "../terr50_gagg_gb_2x.bin")
    DEALLOCATE(shrunkenElev)

    ! **************************************************************************

    ! Shrink by 4x array and save it ...
    CALL sub_shrink_array(                                                      &
                 nx = nx,                                                       &
                 ny = ny,                                                       &
                arr = elev,                                                     &
        shrinkScale = 4_INT64,                                                  &
        shrunkenArr = shrunkenElev                                              &
    )
    CALL sub_save_array_as_BIN(shrunkenElev, "../terr50_gagg_gb_4x.bin")
    DEALLOCATE(shrunkenElev)

    ! **************************************************************************

    ! Shrink by 8x array and save it ...
    CALL sub_shrink_array(                                                      &
                 nx = nx,                                                       &
                 ny = ny,                                                       &
                arr = elev,                                                     &
        shrinkScale = 8_INT64,                                                  &
        shrunkenArr = shrunkenElev                                              &
    )
    CALL sub_save_array_as_BIN(shrunkenElev, "../terr50_gagg_gb_8x.bin")
    DEALLOCATE(shrunkenElev)

    ! **************************************************************************

    ! Shrink by 75x array and save it ...
    CALL sub_shrink_array(                                                      &
                 nx = nx,                                                       &
                 ny = ny,                                                       &
                arr = elev,                                                     &
        shrinkScale = 75_INT64,                                                 &
        shrunkenArr = shrunkenElev                                              &
    )
    CALL sub_save_array_as_BIN(shrunkenElev, "../terr50_gagg_gb_75x.bin")
    DEALLOCATE(shrunkenElev)

    ! **************************************************************************

    ! Clean up ...
    DEALLOCATE(elev)
END PROGRAM main
