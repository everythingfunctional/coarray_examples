program hello_co_broadcast
    implicit none

    character(len=20) :: message
    integer :: me, ni

    me = this_image()
    ni = num_images()
    if (me == ni) then
        write(message, "(A,I0)") "Hello from image ", me
    end if
    call co_broadcast(message, ni)
    critical
        print *, "Received message '" // trim(message) // "' on image ", me
    end critical
end program