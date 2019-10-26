MODULE mod_funcs
    CONTAINS

    PURE SUBROUTINE incrementMask(nx, ny, elev, mask, ixlo, ixhi, iylo, iyhi)
        ! Import modules ...
        USE ISO_FORTRAN_ENV

        IMPLICIT NONE

        ! Declare inputs/outputs ...
        INTEGER(kind = INT64), INTENT(in)                                       :: nx
        INTEGER(kind = INT64), INTENT(in)                                       :: ny
        INTEGER(kind = INT16), DIMENSION(nx, ny), INTENT(inout)                 :: elev
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(inout)                  :: mask
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

        ! Loop over x-axis ...
        DO ix = ixlo, ixhi
            ! Find the limits of the border around this pixel ...
            ix1 = MAX(ix - 1_INT64, ixlo)
            ix2 = MIN(ix + 1_INT64, ixhi)

            ! Loop over y-axis ...
            DO iy = iylo, iyhi
                ! Find the limits of the border around this pixel ...
                iy1 = MAX(iy - 1_INT64, iylo)
                iy2 = MIN(iy + 1_INT64, iyhi)

                ! Check that this pixel has not already been allowed ...
                IF(.NOT. mask(ix, iy))THEN
                    ! Check that this pixel is <= 2,500m ASL ...
                    IF(elev(ix, iy) <= 2500_INT16)THEN
                        ! Check that this pixel is accessible ...
                        IF(ANY(mask(ix1:ix2, iy1:iy2)))THEN
                            ! Allow pregnant women to go here ...
                            mask(ix, iy) = .TRUE._INT8
                        END IF
                    END IF
                END IF
            END DO
        END DO
    END SUBROUTINE incrementMask

    SUBROUTINE saveShrunkMask(nx, ny, mask, scale, bname, iname)
        ! Import modules ...
        USE ISO_FORTRAN_ENV
        USE mod_safe,       ONLY:   sub_allocate_array,                         &
                                    sub_save_array_as_BIN,                      &
                                    sub_save_array_as_PPM

        IMPLICIT NONE

        ! Declare inputs/outputs ...
        INTEGER(kind = INT64), INTENT(in)                                       :: nx
        INTEGER(kind = INT64), INTENT(in)                                       :: ny
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(in)                     :: mask
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
        REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                       :: shrunkMask

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
        CALL sub_allocate_array(shrunkMask, "shrunkMask", nx / scale, ny / scale, .TRUE._INT8)

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

                ! Find average mask ...
                ! NOTE: Within shrunkMask:
                !         *     0.0     = pregnant women can't go here =  RED
                !         * scale*scale = pregnant women  can  go here = GREEN
                shrunkMask(ix, iy) = REAL(COUNT(mask(ixlo:ixhi, iylo:iyhi), kind = INT64), kind = REAL32)
            END DO
        END DO

        ! Convert total mask to average mask ...
        ! NOTE: Within shrunkMask:
        !         * 0.0 = pregnant women can't go here =  RED
        !         * 1.0 = pregnant women  can  go here = GREEN
        shrunkMask = shrunkMask / REAL(scale * scale, kind = REAL32)

        ! Save shrunk mask ...
        CALL sub_save_array_as_BIN(shrunkMask, TRIM(bname))
        CALL sub_save_array_as_PPM(shrunkMask, TRIM(iname), "r2g")

        ! Clean up ...
        DEALLOCATE(shrunkMask)
    END SUBROUTINE saveShrunkMask
END MODULE mod_funcs
