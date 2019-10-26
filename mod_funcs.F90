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

    SUBROUTINE saveShrunkFlood(nx, ny, flooded, scale, bname, iname)
        ! Import modules ...
        USE ISO_FORTRAN_ENV
        USE mod_safe,       ONLY:   sub_allocate_array,                         &
                                    sub_save_array_as_BIN,                      &
                                    sub_save_array_as_PPM

        IMPLICIT NONE

        ! Declare inputs/outputs ...
        INTEGER(kind = INT64), INTENT(in)                                       :: nx
        INTEGER(kind = INT64), INTENT(in)                                       :: ny
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(in)                     :: flooded
        INTEGER(kind = INT64), INTENT(in)                                       :: scale
        CHARACTER(len = *), INTENT(in)                                          :: bname
        CHARACTER(len = *), INTENT(in)                                          :: iname

        ! Declare variables ...
        INTEGER(kind = INT64)                                                   :: ix
        INTEGER(kind = INT64)                                                   :: ixlo
        INTEGER(kind = INT64)                                                   :: ixhi
        INTEGER(kind = INT64)                                                   :: iy
        INTEGER(kind = INT64)                                                   :: iylo
        INTEGER(kind = INT64)                                                   :: iyhi
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

                ! Find total flood ...
                ! NOTE: Within shrunkFlooded:
                !         *     0.0     = not flooded = GREEN
                !         * scale*scale =   flooded   = BLUE
                shrunkFlooded(ix, iy) = REAL(COUNT(flooded(ixlo:ixhi, iylo:iyhi), kind = INT64), kind = REAL32)
            END DO
        END DO

        ! Convert total flood to average flood ...
        ! NOTE: Within shrunkFlooded:
        !         * 0.0 = not flooded = GREEN
        !         * 1.0 =   flooded   = BLUE
        shrunkFlooded = shrunkFlooded / REAL(scale * scale, kind = REAL32)

        ! Save shrunk flood ...
        CALL sub_save_array_as_BIN(shrunkFlooded, TRIM(bname))
        CALL sub_save_array_as_PPM(shrunkFlooded, TRIM(iname), "g2b")

        ! Clean up ...
        DEALLOCATE(shrunkFlooded)
    END SUBROUTINE saveShrunkFlood
END MODULE mod_funcs
