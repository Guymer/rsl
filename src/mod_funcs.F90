MODULE mod_funcs
    CONTAINS

    PURE SUBROUTINE incrementFlood(nx, ny, elev, seaLevel, flooded, ixlo, ixhi, iylo, iyhi)
        ! Import modules ...
        USE ISO_FORTRAN_ENV

        IMPLICIT NONE

        ! Declare inputs/outputs ...
        INTEGER(kind = INT64), INTENT(in)                                       :: nx
        INTEGER(kind = INT64), INTENT(in)                                       :: ny
        REAL(kind = REAL32), DIMENSION(nx, ny), INTENT(inout)                   :: elev
        REAL(kind = REAL32), INTENT(in)                                         :: seaLevel
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(inout)                  :: flooded
        INTEGER(kind = INT64), INTENT(in)                                       :: ixlo
        INTEGER(kind = INT64), INTENT(in)                                       :: ixhi
        INTEGER(kind = INT64), INTENT(in)                                       :: iylo
        INTEGER(kind = INT64), INTENT(in)                                       :: iyhi

        ! Declare variables ...
        INTEGER(kind = INT64)                                                   :: ix
        INTEGER(kind = INT64)                                                   :: ix1
        INTEGER(kind = INT64)                                                   :: ix2
        INTEGER(kind = INT64)                                                   :: iy
        INTEGER(kind = INT64)                                                   :: iy1
        INTEGER(kind = INT64)                                                   :: iy2

        ! NOTE: The arrays go:
        !       ( 1, 1) ... (nx, 1)
        !         ...         ...
        !       ( 1,ny) ... (nx,ny)

        ! **********************************************************************
        ! NOTE: Start top-left and go down then right.
        ! **********************************************************************

        ! Loop over x-axis ...
        DO ix = ixlo, ixhi, 1_INT64
            ! Find the limits of the border around this pixel ...
            ix1 = MAX(ix - 1_INT64, ixlo)
            ix2 = MIN(ix + 1_INT64, ixhi)

            ! Loop over y-axis ...
            DO iy = iylo, iyhi, 1_INT64
                ! Find the limits of the border around this pixel ...
                iy1 = MAX(iy - 1_INT64, iylo)
                iy2 = MIN(iy + 1_INT64, iyhi)

                ! Check that this pixel has not already been flooded ...
                IF(.NOT. flooded(ix, iy))THEN
                    ! Check that this pixel is <= sea level ...
                    IF(elev(ix, iy) <= seaLevel)THEN
                        ! Check that this pixel is next to a flooded pixel ...
                        IF(ANY(flooded(ix1:ix2, iy1:iy2)))THEN
                            ! Flood this pixel ...
                            ! NOTE: Within flooded:
                            !         * .FALSE. = not flooded
                            !         * .TRUE.  = flooded
                            flooded(ix, iy) = .TRUE._INT8
                        END IF
                    END IF
                END IF
            END DO
        END DO

        ! **********************************************************************
        ! NOTE: Start top-right and go left then down.
        ! **********************************************************************

        ! Loop over y-axis ...
        DO iy = iylo, iyhi, 1_INT64
            ! Find the limits of the border around this pixel ...
            iy1 = MAX(iy - 1_INT64, iylo)
            iy2 = MIN(iy + 1_INT64, iyhi)

            ! Loop over x-axis ...
            DO ix = ixhi, ixlo, -1_INT64
                ! Find the limits of the border around this pixel ...
                ix1 = MAX(ix - 1_INT64, ixlo)
                ix2 = MIN(ix + 1_INT64, ixhi)

                ! Check that this pixel has not already been flooded ...
                IF(.NOT. flooded(ix, iy))THEN
                    ! Check that this pixel is <= sea level ...
                    IF(elev(ix, iy) <= seaLevel)THEN
                        ! Check that this pixel is next to a flooded pixel ...
                        IF(ANY(flooded(ix1:ix2, iy1:iy2)))THEN
                            ! Flood this pixel ...
                            ! NOTE: Within flooded:
                            !         * .FALSE. = not flooded
                            !         * .TRUE.  = flooded
                            flooded(ix, iy) = .TRUE._INT8
                        END IF
                    END IF
                END IF
            END DO
        END DO

        ! **********************************************************************
        ! NOTE: Start bottom-right and go up then left.
        ! **********************************************************************

        ! Loop over x-axis ...
        DO ix = ixhi, ixlo, -1_INT64
            ! Find the limits of the border around this pixel ...
            ix1 = MAX(ix - 1_INT64, ixlo)
            ix2 = MIN(ix + 1_INT64, ixhi)

            ! Loop over y-axis ...
            DO iy = iyhi, iylo, -1_INT64
                ! Find the limits of the border around this pixel ...
                iy1 = MAX(iy - 1_INT64, iylo)
                iy2 = MIN(iy + 1_INT64, iyhi)

                ! Check that this pixel has not already been flooded ...
                IF(.NOT. flooded(ix, iy))THEN
                    ! Check that this pixel is <= sea level ...
                    IF(elev(ix, iy) <= seaLevel)THEN
                        ! Check that this pixel is next to a flooded pixel ...
                        IF(ANY(flooded(ix1:ix2, iy1:iy2)))THEN
                            ! Flood this pixel ...
                            ! NOTE: Within flooded:
                            !         * .FALSE. = not flooded
                            !         * .TRUE.  = flooded
                            flooded(ix, iy) = .TRUE._INT8
                        END IF
                    END IF
                END IF
            END DO
        END DO

        ! **********************************************************************
        ! NOTE: Start bottom-left and go right then up.
        ! **********************************************************************

        ! Loop over y-axis ...
        DO iy = iyhi, iylo, -1_INT64
            ! Find the limits of the border around this pixel ...
            iy1 = MAX(iy - 1_INT64, iylo)
            iy2 = MIN(iy + 1_INT64, iyhi)

            ! Loop over x-axis ...
            DO ix = ixlo, ixhi, 1_INT64
                ! Find the limits of the border around this pixel ...
                ix1 = MAX(ix - 1_INT64, ixlo)
                ix2 = MIN(ix + 1_INT64, ixhi)

                ! Check that this pixel has not already been flooded ...
                IF(.NOT. flooded(ix, iy))THEN
                    ! Check that this pixel is <= sea level ...
                    IF(elev(ix, iy) <= seaLevel)THEN
                        ! Check that this pixel is next to a flooded pixel ...
                        IF(ANY(flooded(ix1:ix2, iy1:iy2)))THEN
                            ! Flood this pixel ...
                            ! NOTE: Within flooded:
                            !         * .FALSE. = not flooded
                            !         * .TRUE.  = flooded
                            flooded(ix, iy) = .TRUE._INT8
                        END IF
                    END IF
                END IF
            END DO
        END DO
    END SUBROUTINE incrementFlood

    SUBROUTINE saveArray(shrunkAtRisk, shrunkFlooded, fname)
        ! NOTE: This is a copy of "sub_save_2D_REAL32_real_array_as_PPM()" from
        !       "mod_safe", the only modification is to be hard-coded to be
        !       indexed to either red, green or blue:
        !         * red = below sea level but not flooded (i.e., at risk of
        !                 flooding);
        !         * green = above sea level; and
        !         * blue = flooded.

        ! Import modules ...
        USE ISO_FORTRAN_ENV

        IMPLICIT NONE

        ! Declare inputs/outputs ...
        CHARACTER(len = *), INTENT(in)                                          :: fname
        REAL(kind = REAL32), DIMENSION(:, :), INTENT(in)                        :: shrunkAtRisk
        REAL(kind = REAL32), DIMENSION(:, :), INTENT(in)                        :: shrunkFlooded

        ! Declare FORTRAN variables ...
        CHARACTER(len = 256)                                                    :: errmsg
        INTEGER(kind = INT32)                                                   :: errnum
        INTEGER(kind = INT32)                                                   :: funit

        ! Declare internal variables ...
        CHARACTER(len = 3), ALLOCATABLE, DIMENSION(:, :)                        :: img
        CHARACTER(len = 19)                                                     :: hdr
        INTEGER(kind = INT64)                                                   :: ix
        INTEGER(kind = INT64)                                                   :: iy
        INTEGER(kind = INT64)                                                   :: nx
        INTEGER(kind = INT64)                                                   :: ny

        ! Find size of image ...
        nx = SIZE(shrunkFlooded, dim = 1, kind = INT64)
        ny = SIZE(shrunkFlooded, dim = 2, kind = INT64)

        ! Make header ...
        WRITE(hdr, fmt = '("P6 ", i5, " ", i5, " 255 ")') nx, ny

        ! Allocate image ...
        ALLOCATE(img(nx, ny))

        ! Loop over x ...
        DO ix = 1_INT64, nx
            ! Loop over y ...
            DO iy = 1_INT64, ny
                ! Set pixel ...
                IF(shrunkFlooded(ix, iy) >= 0.5e0_REAL32)THEN
                    img(ix, iy) = ACHAR(  0) // ACHAR(  0) // ACHAR(255)
                ELSE IF(shrunkAtRisk(ix, iy) >= 0.5e0_REAL32)THEN
                    img(ix, iy) = ACHAR(255) // ACHAR(  0) // ACHAR(  0)
                ELSE
                    img(ix, iy) = ACHAR(  0) // ACHAR(255) // ACHAR(  0)
                END IF
            END DO
        END DO

        ! Open PPM ...
        OPEN(                                                                   &
             access = "stream",                                                 &
             action = "write",                                                  &
               file = TRIM(fname),                                              &
               form = "unformatted",                                            &
              iomsg = errmsg,                                                   &
             iostat = errnum,                                                   &
            newunit = funit,                                                    &
             status = "replace"                                                 &
        )
        IF(errnum /= 0_INT32)THEN
            WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to open PPM", TRIM(errmsg), errnum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Write out header and image ...
        WRITE(unit = funit) hdr
        WRITE(unit = funit) img

        ! Close PPM ...
        CLOSE(unit = funit)

        ! Clean up ...
        DEALLOCATE(img)
    END SUBROUTINE saveArray

    SUBROUTINE saveShrunkFlood(nx, ny, atRisk, flooded, scale, iname)
        ! Import modules ...
        USE ISO_FORTRAN_ENV
        USE mod_safe,       ONLY:   sub_allocate_array


        IMPLICIT NONE

        ! Declare inputs/outputs ...
        INTEGER(kind = INT64), INTENT(in)                                       :: nx
        INTEGER(kind = INT64), INTENT(in)                                       :: ny
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(in)                     :: atRisk
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(in)                     :: flooded
        INTEGER(kind = INT64), INTENT(in)                                       :: scale
        CHARACTER(len = *), INTENT(in)                                          :: iname

        ! Declare variables ...
        INTEGER(kind = INT64)                                                   :: ix
        INTEGER(kind = INT64)                                                   :: ixlo
        INTEGER(kind = INT64)                                                   :: ixhi
        INTEGER(kind = INT64)                                                   :: iy
        INTEGER(kind = INT64)                                                   :: iylo
        INTEGER(kind = INT64)                                                   :: iyhi
        REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                       :: shrunkAtRisk
        REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                       :: shrunkFlooded

        ! Check scale ...
        IF(MOD(nx, scale) /= 0_INT64)THEN
            WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"nx" is not an integer multiple of "scale"'
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF
        IF(MOD(ny, scale) /= 0_INT64)THEN
            WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"ny" is not an integer multiple of "scale"'
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Allocate array ...
        CALL sub_allocate_array(shrunkAtRisk, "shrunkAtRisk", nx / scale, ny / scale, .TRUE._INT8)
        CALL sub_allocate_array(shrunkFlooded, "shrunkFlooded", nx / scale, ny / scale, .TRUE._INT8)

        ! Loop over x-axis tiles ...
        DO ix = 1_INT64, nx / scale
            ! Find the extent of the tile ...
            ixlo = (ix - 1_INT64) * scale + 1_INT64
            ixhi =  ix            * scale

            ! Loop over y-axis tiles ...
            DO iy = 1_INT64, ny / scale
                ! Find the extent of the tile ...
                iylo = (iy - 1_INT64) * scale + 1_INT64
                iyhi =  iy            * scale

                ! Find total risk ...
                ! NOTE: Within shrunkAtRisk:
                !         *     0.0     = not at risk = GREEN/BLUE
                !         * scale*scale =   at risk   = RED
                shrunkAtRisk(ix, iy) = REAL(COUNT(atRisk(ixlo:ixhi, iylo:iyhi), kind = INT64), kind = REAL32)

                ! Find total flood ...
                ! NOTE: Within shrunkFlooded:
                !         *     0.0     = not flooded = GREEN/RED
                !         * scale*scale =   flooded   = BLUE
                shrunkFlooded(ix, iy) = REAL(COUNT(flooded(ixlo:ixhi, iylo:iyhi), kind = INT64), kind = REAL32)
            END DO
        END DO

        ! Convert total risk to average risk ...
        ! NOTE: Within shrunkAtRisk:
        !         * 0.0 = not at risk = GREEN/BLUE
        !         * 1.0 =   at risk   = RED
        shrunkAtRisk = shrunkAtRisk / REAL(scale * scale, kind = REAL32)

        ! Convert total flood to average flood ...
        ! NOTE: Within shrunkFlooded:
        !         * 0.0 = not flooded = GREEN
        !         * 1.0 =   flooded   = BLUE
        shrunkFlooded = shrunkFlooded / REAL(scale * scale, kind = REAL32)

        ! Save shrunk flood ...
        CALL saveArray(shrunkAtRisk, shrunkFlooded, iname)

        ! Clean up ...
        DEALLOCATE(shrunkAtRisk)
        DEALLOCATE(shrunkFlooded)
    END SUBROUTINE saveShrunkFlood
END MODULE mod_funcs
