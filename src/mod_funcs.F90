MODULE mod_funcs
    CONTAINS

    SUBROUTINE saveArray(nx, ny, shrunkAtRisk, shrunkFlooded, imageName)
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
        INTEGER(kind = INT64), INTENT(in)                                       :: nx
        INTEGER(kind = INT64), INTENT(in)                                       :: ny
        REAL(kind = REAL32), DIMENSION(nx, ny), INTENT(in)                      :: shrunkAtRisk
        REAL(kind = REAL32), DIMENSION(nx, ny), INTENT(in)                      :: shrunkFlooded
        CHARACTER(len = *), INTENT(in)                                          :: imageName

        ! Declare FORTRAN variables ...
        CHARACTER(len = 256)                                                    :: errmsg
        INTEGER(kind = INT32)                                                   :: errnum
        INTEGER(kind = INT32)                                                   :: funit

        ! Declare internal variables ...
        CHARACTER(len = 3), ALLOCATABLE, DIMENSION(:, :)                        :: img
        CHARACTER(len = 19)                                                     :: hdr
        INTEGER(kind = INT64)                                                   :: ix
        INTEGER(kind = INT64)                                                   :: iy

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
               file = TRIM(imageName),                                          &
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

    SUBROUTINE saveShrunkFlood(nx, ny, atRisk, flooded, imageScale, imageName)
        ! Import standard modules ...
        USE ISO_FORTRAN_ENV

        ! Import my modules ...
        USE mod_safe,       ONLY:   sub_allocate_array,                         &
                                    sub_shrink_array

        IMPLICIT NONE

        ! Declare inputs/outputs ...
        INTEGER(kind = INT64), INTENT(in)                                       :: nx
        INTEGER(kind = INT64), INTENT(in)                                       :: ny
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(in)                     :: atRisk
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(in)                     :: flooded
        INTEGER(kind = INT64), INTENT(in)                                       :: imageScale
        CHARACTER(len = *), INTENT(in)                                          :: imageName

        ! Declare variables ...
        REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                       :: shrunkAtRisk
        REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                       :: shrunkFlooded

        ! Shrink the logical arrays down to real arrays ..
        CALL sub_shrink_array(                                                  &
                     nx = nx,                                                   &
                     ny = ny,                                                   &
                    arr = atRisk,                                               &
            shrinkScale = imageScale,                                           &
            shrunkenArr = shrunkAtRisk                                          &
        )
        CALL sub_shrink_array(                                                  &
                     nx = nx,                                                   &
                     ny = ny,                                                   &
                    arr = flooded,                                              &
            shrinkScale = imageScale,                                           &
            shrunkenArr = shrunkFlooded                                         &
        )

        ! Save shrunk flood ...
        CALL saveArray(                                                         &
                       nx = nx / imageScale,                                    &
                       ny = ny / imageScale,                                    &
             shrunkAtRisk = shrunkAtRisk,                                       &
            shrunkFlooded = shrunkFlooded,                                      &
                imageName = imageName                                           &
        )

        ! Clean up ...
        DEALLOCATE(shrunkAtRisk)
        DEALLOCATE(shrunkFlooded)
    END SUBROUTINE saveShrunkFlood
END MODULE mod_funcs
