PROGRAM main
    ! Import modules ...
    USE ISO_FORTRAN_ENV
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_PPM

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 13200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 24600_INT64

    ! Declare variables ...
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: elev

    ! Allocate (1.21 GiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx, ny, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../terr50_gagg_gb.bin")                 ! [m]

    ! Scale map (up to 1,300m ASL) ...
    elev = elev / 1300.0e0_REAL32

    ! Save scaled map ...
    CALL sub_save_array_as_PPM(elev, "../terr50_gagg_gb.ppm", "jet")

    ! Clean up ...
    DEALLOCATE(elev)

    ! Allocate (309.68 MiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx / 2_INT64, ny / 2_INT64, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../terr50_gagg_gb_2x.bin")              ! [m]

    ! Scale map (up to 1,300m ASL) ...
    elev = elev / 1300.0e0_REAL32

    ! Save scaled map ...
    CALL sub_save_array_as_PPM(elev, "../terr50_gagg_gb_2x.ppm", "jet")

    ! Clean up ...
    DEALLOCATE(elev)

    ! Allocate (77.42 MiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx / 4_INT64, ny / 4_INT64, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../terr50_gagg_gb_4x.bin")              ! [m]

    ! Scale map (up to 1,300m ASL) ...
    elev = elev / 1300.0e0_REAL32

    ! Save scaled map ...
    CALL sub_save_array_as_PPM(elev, "../terr50_gagg_gb_4x.ppm", "jet")

    ! Clean up ...
    DEALLOCATE(elev)

    ! Allocate (19.35 MiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx / 8_INT64, ny / 8_INT64, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../terr50_gagg_gb_8x.bin")              ! [m]

    ! Scale map (up to 1,300m ASL) ...
    elev = elev / 1300.0e0_REAL32

    ! Save scaled map ...
    CALL sub_save_array_as_PPM(elev, "../terr50_gagg_gb_8x.ppm", "jet")

    ! Clean up ...
    DEALLOCATE(elev)

    ! Allocate (126.84 KiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx / 75_INT64, ny / 75_INT64, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../terr50_gagg_gb_75x.bin")            ! [m]

    ! Scale map (up to 1,300m ASL) ...
    elev = elev / 1300.0e0_REAL32

    ! Save scaled map ...
    CALL sub_save_array_as_PPM(elev, "../terr50_gagg_gb_75x.ppm", "jet")

    ! Clean up ...
    DEALLOCATE(elev)
END PROGRAM main
