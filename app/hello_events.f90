program hello_sync_images
    use iso_fortran_env, only: event_type

    implicit none

    character(len=20) :: message[*]
    type(event_type) :: message_ready[*]
    integer :: me, ni

    me = this_image()
    ni = num_images()
    if (me == ni) then
        write(message, "(A,I0)") "Hello from image ", me
        if (me > 1) event post (message_ready[me-1])
    else
        event wait (message_ready)
        message = message[me+1]
        if (me > 1) event post (message_ready[me-1])
    end if
    critical
        print *, "Received message '" // trim(message) // "' on image ", me
    end critical
end program