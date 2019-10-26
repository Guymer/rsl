PROGRAM main
    ! Import modules ...
    USE ISO_FORTRAN_ENV
    USE mod_funcs
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nmax = 50_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: nmax_thread = 1000_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 13200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 24600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: imageScale = 10_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: tileScale = 50_INT64

    ! Declare variables ...
    CHARACTER(len = 24)                                                         :: bname
    CHARACTER(len = 24)                                                         :: iname
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: flooded
    INTEGER(kind = INT64)                                                       :: i
    INTEGER(kind = INT64)                                                       :: i_thread
    INTEGER(kind = INT64)                                                       :: ix
    INTEGER(kind = INT64)                                                       :: ixlo
    INTEGER(kind = INT64)                                                       :: ixhi
    INTEGER(kind = INT64)                                                       :: iy
    INTEGER(kind = INT64)                                                       :: iylo
    INTEGER(kind = INT64)                                                       :: iyhi
    INTEGER(kind = INT64)                                                       :: newtot
    INTEGER(kind = INT64)                                                       :: newtot_thread
    INTEGER(kind = INT64)                                                       :: oldtot
    INTEGER(kind = INT64)                                                       :: oldtot_thread
    REAL(kind = REAL32)                                                         :: seaLevel
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: elev

    ! Declare FORTRAN variables ...
    CHARACTER(len = 256)                                                        :: errmsg
    INTEGER(kind = INT32)                                                       :: errnum
    INTEGER(kind = INT32)                                                       :: funit

    ! Check imageScale ...
    IF(MOD(nx, imageScale) /= 0_INT64)THEN
        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"nx" is not an integer multiple of "imageScale"'
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF
    IF(MOD(ny, imageScale) /= 0_INT64)THEN
        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"ny" is not an integer multiple of "imageScale"'
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Check tileScale ...
    IF(MOD(nx, tileScale) /= 0_INT64)THEN
        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"nx" is not an integer multiple of "tileScale"'
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF
    IF(MOD(ny, tileScale) /= 0_INT64)THEN
        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"ny" is not an integer multiple of "tileScale"'
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Allocate (1.21 GiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx, ny, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "terr50_gagg_gb.bin")                    ! [m]

    ! Allocate (309.68 MiB) array and initialize it so that nowhere is flooded ...
    CALL sub_allocate_array(flooded, "flooded", nx, ny, .TRUE._INT8)
    flooded = .FALSE._INT8

    ! Flood the top-left corner ...
    flooded(1, 1) = .TRUE._INT8

    ! Set sea level ...
    seaLevel = 0.0e0_REAL32                                                     ! [m]

    ! Open CSV ...
    OPEN(action = "write", file = "createFlood.csv", form = "formatted", iomsg = errmsg, iostat = errnum, newunit = funit, status = "replace")
    IF(errnum /= 0_INT32)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to open BIN", TRIM(errmsg), errnum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Write header ...
    WRITE(fmt = '(a)', unit = funit) "step,pixels flooded"
    FLUSH(unit = funit)

    ! Start ~infinite loop ...
    DO i = 1_INT64, nmax
        ! Print progress ...
        WRITE(fmt = '("Calculating step ", i4, " of (up to) ", i4, " ...")', unit = OUTPUT_UNIT) i, nmax
        FLUSH(unit = OUTPUT_UNIT)

        ! Create file names ...
        WRITE(bname, '("createFlood_step", i4.4, ".bin")') i
        WRITE(iname, '("createFlood_step", i4.4, ".ppm")') i

        ! Find initial total ...
        oldtot = COUNT(flooded, kind = INT64)

        ! Increment flood (of the UK) ...
        CALL incrementFlood(nx, ny, elev, seaLevel, flooded, 1_INT64, nx, 1_INT64, ny)

        !$omp parallel                                                          &
        !$omp default(none)                                                     &
        !$omp private(i_thread)                                                 &
        !$omp private(ix)                                                       &
        !$omp private(ixlo)                                                     &
        !$omp private(ixhi)                                                     &
        !$omp private(iy)                                                       &
        !$omp private(iylo)                                                     &
        !$omp private(iyhi)                                                     &
        !$omp private(oldtot_thread)                                            &
        !$omp private(newtot_thread)                                            &
        !$omp shared(elev)                                                      &
        !$omp shared(flooded)                                                   &
        !$omp shared(seaLevel)
            !$omp do                                                            &
            !$omp schedule(dynamic)
                ! Loop over x-axis tiles ...
                DO ix = 1_INT64, nx / tileScale
                    ! Find the extent of the tile ...
                    ixlo = (ix - 1_INT64) * tileScale + 1_INT64
                    ixhi =  ix            * tileScale

                    ! Loop over y-axis tiles ...
                    DO iy = 1_INT64, ny / tileScale
                        ! Find the extent of the tile ...
                        iylo = (iy - 1_INT64) * tileScale + 1_INT64
                        iyhi =  iy            * tileScale

                        ! Start ~infinite loop ...
                        DO i_thread = 1_INT64, nmax_thread
                            ! Find initial total ...
                            oldtot_thread = COUNT(flooded(ixlo:ixhi, iylo:iyhi), kind = INT64)

                            ! Stop looping once no changes can be made ...
                            IF(oldtot_thread == 0_INT64 .OR. oldtot_thread == tileScale ** 2)THEN
                                EXIT
                            END IF

                            ! Increment flood (of the tile) ...
                            CALL incrementFlood(nx, ny, elev, seaLevel, flooded, ixlo, ixhi, iylo, iyhi)

                            ! Find new total ...
                            newtot_thread = COUNT(flooded(ixlo:ixhi, iylo:iyhi), kind = INT64)

                            ! Stop looping once no changes have been made ...
                            IF(newtot_thread == oldtot_thread)THEN
                                EXIT
                            END IF
                        END DO
                    END DO
                END DO
            !$omp end do
        !$omp end parallel

        ! Find new total ...
        newtot = COUNT(flooded, kind = INT64)

        ! Write progress ...
        WRITE(fmt = '(i2, ",", i9)', unit = funit) i, newtot
        FLUSH(unit = funit)

        ! Stop looping once no changes have been made ...
        IF(newtot == oldtot)THEN
            EXIT
        END IF
    END DO

    ! Close CSV ...
    CLOSE(unit = funit)

    ! Print progress ...
    WRITE(fmt = '("Saving final answer ...")', unit = OUTPUT_UNIT)
    FLUSH(unit = OUTPUT_UNIT)

    ! Save shrunk final flood ...
    CALL saveShrunkFlood(nx, ny, flooded, imageScale, bname, iname)

    ! Clean up ...
    DEALLOCATE(elev)
    DEALLOCATE(flooded)
END PROGRAM main
