MODULE mod_funcs
    CONTAINS

    SUBROUTINE saveArray(nx, ny, shrunkAtRisk, shrunkFlooded, imageName)
        ! Import standard modules ...
        USE ISO_FORTRAN_ENV

        ! Import my modules ...
        USE mod_safe,       ONLY:   sub_allocate_array,                         &
                                    sub_save_array_as_BIN

        IMPLICIT NONE

        ! Declare inputs/outputs ...
        INTEGER(kind = INT64), INTENT(in)                                       :: nx
        INTEGER(kind = INT64), INTENT(in)                                       :: ny
        REAL(kind = REAL32), DIMENSION(nx, ny), INTENT(in)                      :: shrunkAtRisk
        REAL(kind = REAL32), DIMENSION(nx, ny), INTENT(in)                      :: shrunkFlooded
        CHARACTER(len = *), INTENT(in)                                          :: imageName

        ! Declare internal variables ...
        INTEGER(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                      :: shrunkImage
        INTEGER(kind = INT64)                                                   :: ix
        INTEGER(kind = INT64)                                                   :: iy

        ! Allocate image ...
        CALL sub_allocate_array(                                                &
            shrunkImage,                                                        &
            "shrunkImage",                                                      &
            nx,                                                                 &
            ny,                                                                 &
            .TRUE._INT8                                                         &
        )

        ! Loop over x ...
        DO ix = 1_INT64, nx
            ! Loop over y ...
            DO iy = 1_INT64, ny
                ! Set pixel ...
                ! NOTE: If the average amount of flooding is ≥ 50% then the
                !       pixel is flooded. If the average amount of risk is ≥ 50%
                !       (whilst not being flooded) then the pixel is at risk.
                !       Else the pixel is safe.
                IF(shrunkFlooded(ix, iy) >= 0.5e0_REAL32)THEN
                    shrunkImage(ix, iy) = 0_INT8                                ! Blue
                ELSE IF(shrunkAtRisk(ix, iy) >= 0.5e0_REAL32)THEN
                    shrunkImage(ix, iy) = 1_INT8                                ! Red
                ELSE
                    shrunkImage(ix, iy) = 2_INT8                                ! Green
                END IF
            END DO
        END DO

        ! Save image ...
        CALL sub_save_array_as_BIN(                                             &
            shrunkImage,                                                        &
            TRIM(imageName)                                                     &
        )

        ! Clean up ...
        DEALLOCATE(shrunkImage)
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
