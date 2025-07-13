PROGRAM main
    ! Import standard modules ...
    USE ISO_FORTRAN_ENV

    ! Import my modules ...
    USE mod_funcs,          ONLY:   saveShrunkFlood
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_flood_array,                            &
                                    sub_load_array_from_BIN

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 13200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 24600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: imageScale = 10_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: tileScale = 50_INT64

    ! Declare variables ...
    CHARACTER(len = 256)                                                        :: imageName
    CHARACTER(len = 256)                                                        :: logName
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: atRisk
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: flooded
    INTEGER(kind = INT64)                                                       :: iIter
    INTEGER(kind = INT64)                                                       :: iSeaLevel
    INTEGER(kind = INT64), ALLOCATABLE, DIMENSION(:)                            :: tot
    REAL(kind = REAL32)                                                         :: seaLevel
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: elev

    ! Declare FORTRAN variables ...
    CHARACTER(len = 256)                                                        :: errmsg
    INTEGER(kind = INT32)                                                       :: errnum
    INTEGER(kind = INT32)                                                       :: funit

    ! **************************************************************************

    ! Ensure that the output directory exists ...
    CALL EXECUTE_COMMAND_LINE("mkdir -p ../output", CMDMSG = errmsg, EXITSTAT = errnum)
    IF(errnum /= 0_INT32)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to make output directory", TRIM(errmsg), errnum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Allocate (1.21 GiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx, ny, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../terr50_gagg_gb.bin")                 ! [m]

    ! Allocate (309.68 MiB) array ...
    CALL sub_allocate_array(atRisk, "atRisk", nx, ny, .TRUE._INT8)

    ! Allocate (309.68 MiB) array and initialize it so that nowhere is flooded
    ! apart from the top-left corner ...
    CALL sub_allocate_array(flooded, "flooded", nx, ny, .TRUE._INT8)
    flooded = .FALSE._INT8
    flooded(1, 1) = .TRUE._INT8

    ! Loop over sea levels ...
    DO iSeaLevel = 0_INT64, NINT(MAXVAL(elev), kind = INT64), 1_INT64
        ! Print progress ...
        WRITE(fmt = '("Calculating a sea level rise of ", i4, "m ...")', unit = OUTPUT_UNIT) iSeaLevel
        FLUSH(unit = OUTPUT_UNIT)

        ! Set sea level and find out where is at risk of flooding ...
        seaLevel = REAL(iSeaLevel, kind = REAL32)                               ! [m]
        atRisk = elev <= seaLevel

        ! Raise the sea level ...
        CALL sub_flood_array(                                                   &
                   nx = nx,                                                     &
                   ny = ny,                                                     &
                 elev = elev,                                                   &
             seaLevel = seaLevel,                                               &
              flooded = flooded,                                                &
            tileScale = tileScale,                                              &
                  tot = tot                                                     &
        )

        ! Create file name, write out the convergence and clean up ...
        WRITE(logName, '("../output/", i4.4, "_", i3.3, "x.csv")') iSeaLevel, imageScale
        OPEN(                                                                   &
             action = "write",                                                  &
               file = TRIM(logName),                                            &
               form = "formatted",                                              &
              iomsg = errmsg,                                                   &
             iostat = errnum,                                                   &
            newunit = funit,                                                    &
             status = "replace"                                                 &
        )
        IF(errnum /= 0_INT32)THEN
            WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to open CSV", TRIM(errmsg), errnum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF
        WRITE(fmt = '(a)', unit = funit) "iteration,pixels allowed"
        FLUSH(unit = funit)
        DO iIter = LBOUND(tot, dim = 1, kind = INT64), UBOUND(tot, dim = 1, kind = INT64), 1_INT64
            IF(tot(iIter) == 0_INT64)THEN
                EXIT
            END IF
            WRITE(fmt = '(i3, ",", i9)', unit = funit) iIter, tot(iIter)
            FLUSH(unit = funit)
        END DO
        CLOSE(unit = funit)
        DEALLOCATE(tot)

        ! Create file name and save shrunk final flood ...
        WRITE(imageName, '("../output/", i4.4, "m_", i3.3, "x.bin")') iSeaLevel, imageScale
        CALL saveShrunkFlood(                                                   &
                    nx = nx,                                                    &
                    ny = ny,                                                    &
                atRisk = atRisk,                                                &
               flooded = flooded,                                               &
            imageScale = imageScale,                                            &
             imageName = TRIM(imageName)                                        &
        )
    END DO

    ! Clean up ...
    DEALLOCATE(elev)
    DEALLOCATE(atRisk)
    DEALLOCATE(flooded)
END PROGRAM main
