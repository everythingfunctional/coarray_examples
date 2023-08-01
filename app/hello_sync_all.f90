program hello_sync_all
    implicit none

    character(len=20) :: message[*]
    integer :: me, ni

    me = this_image()
    ni = num_images()
    if (me == ni) then
        write(message, "(A,I0)") "Hello from image ", me
    end if
    sync all
    critical
        print *, "Received message '" // trim(message[ni]) // "' on image ", me
    end critical
end program