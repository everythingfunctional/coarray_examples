program hello_sync_images
    implicit none

    character(len=20) :: message[*]
    integer :: me, ni

    me = this_image()
    ni = num_images()
    if (me == ni) then
        write(message, "(A,I0)") "Hello from image ", me
        if (me > 1) sync images ([me-1])
    else
        sync images ([me+1])
        message = message[me+1]
        if (me > 1) sync images ([me-1])
    end if
    critical
        print *, "Received message '" // trim(message) // "' on image ", me
    end critical
end program