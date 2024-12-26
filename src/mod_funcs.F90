MODULE mod_funcs
    CONTAINS

    SUBROUTINE saveArray(shrunkAtRisk, shrunkFlooded, fname)
        ! NOTE: This is a copy of "sub_save_2D_REAL32_real_array_as_PPM()" from
        !       "mod_safe", the only modification is to be hard-coded to be
        !       indexed to either red, green or blue:
        !         * red = below sea level but not flooded (i.e., at risk of
        !                 flooding);
        !         * green = above sea level; and
        !         * blue = flooded.

        ! Import standard modules ...
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
        ! NOTE: There is no "sub_allocate_array()" for CHARACTER arrays.
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

    SUBROUTINE saveShrunkFlood(nx, ny, atRisk, flooded, imageScale, iname)
        ! Import standard modules ...
        USE ISO_FORTRAN_ENV

        ! Import my modules ...
        USE mod_safe,       ONLY:   sub_allocate_array

        IMPLICIT NONE

        ! Declare inputs/outputs ...
        INTEGER(kind = INT64), INTENT(in)                                       :: nx
        INTEGER(kind = INT64), INTENT(in)                                       :: ny
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(in)                     :: atRisk
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(in)                     :: flooded
        INTEGER(kind = INT64), INTENT(in)                                       :: imageScale
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

        ! Allocate array ...
        CALL sub_allocate_array(shrunkAtRisk, "shrunkAtRisk", nx / imageScale, ny / imageScale, .TRUE._INT8)
        CALL sub_allocate_array(shrunkFlooded, "shrunkFlooded", nx / imageScale, ny / imageScale, .TRUE._INT8)

        ! Loop over x-axis tiles ...
        DO ix = 1_INT64, nx / imageScale
            ! Find the extent of the tile ...
            ixlo = (ix - 1_INT64) * imageScale + 1_INT64
            ixhi =  ix            * imageScale

            ! Loop over y-axis tiles ...
            DO iy = 1_INT64, ny / imageScale
                ! Find the extent of the tile ...
                iylo = (iy - 1_INT64) * imageScale + 1_INT64
                iyhi =  iy            * imageScale

                ! Find total risk ...
                ! NOTE: Within shrunkAtRisk:
                !         *          0.0          = not at risk = GREEN/BLUE
                !         * imageScale*imageScale =   at risk   = RED
                shrunkAtRisk(ix, iy) = REAL(COUNT(atRisk(ixlo:ixhi, iylo:iyhi), kind = INT64), kind = REAL32)

                ! Find total flood ...
                ! NOTE: Within shrunkFlooded:
                !         *          0.0          = not flooded = GREEN/RED
                !         * imageScale*imageScale =   flooded   = BLUE
                shrunkFlooded(ix, iy) = REAL(COUNT(flooded(ixlo:ixhi, iylo:iyhi), kind = INT64), kind = REAL32)
            END DO
        END DO

        ! Convert total risk to average risk ...
        ! NOTE: Within shrunkAtRisk:
        !         * 0.0 = not at risk = GREEN/BLUE
        !         * 1.0 =   at risk   = RED
        shrunkAtRisk = shrunkAtRisk / REAL(imageScale * imageScale, kind = REAL32)

        ! Convert total flood to average flood ...
        ! NOTE: Within shrunkFlooded:
        !         * 0.0 = not flooded = GREEN/RED
        !         * 1.0 =   flooded   = BLUE
        shrunkFlooded = shrunkFlooded / REAL(imageScale * imageScale, kind = REAL32)

        ! Save shrunk flood ...
        CALL saveArray(shrunkAtRisk, shrunkFlooded, iname)

        ! Clean up ...
        DEALLOCATE(shrunkAtRisk)
        DEALLOCATE(shrunkFlooded)
    END SUBROUTINE saveShrunkFlood
END MODULE mod_funcs
