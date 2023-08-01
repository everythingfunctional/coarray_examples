program hello_from_everyone
    use iso_fortran_env, only: event_type
    use stdlib_system, only: sleep

    implicit none

    character(len=23) :: message[*]
    type(event_type), allocatable :: message_ready(:)[:]
    type(event_type) :: can_overwrite[*]
    integer :: me, ni, i, j, count
    logical, allocatable :: printed(:)
    real :: sleep_time

    me = this_image()
    ni = num_images()
    allocate(message_ready(ni)[*])
    allocate(printed(ni))

    if (me == 1) then
        do i = 1, ni
            event post (can_overwrite[i])
        end do
    end if
    do i = 1, 10
        event wait (can_overwrite)
        write(message, "(A,I0,A,I0)") "Hello ", i, " from image ", me
        if (me /= 1) then
            ! sleep for up to 2 seconds to simulate some work to be done
            call random_number(sleep_time)
            call sleep(int(sleep_time*2000))
        end if
        event post (message_ready(me)[1])
        if (me == 1) then
            printed = .false.
            do while (.not. all(printed))
                do j = 1, ni
                    if (.not. printed(j)) then
                        call event_query(message_ready(j), count)
                        if (count > 0) then
                            event wait (message_ready(j))
                            print *, message[j]
                            event post (can_overwrite[j])
                            printed(j) = .true.
                        end if
                    end if
                end do
            end do
        end if
    end do
    event wait (can_overwrite)
end program