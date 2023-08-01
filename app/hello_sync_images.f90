program hello_sync_images
    implicit none

    character(len=20) :: message[*]
    integer :: me, ni

    me = this_image()
    ni = num_images()
    if (me == ni) then
        write(message, "(A,I0)") "Hello from image ", me
    else
        sync images ([me+1])
        message = message[me+1]
    end if
    if (me > 1) sync images ([me-1])
    critical
        print *, "Received message '" // trim(message) // "' on image ", me
    end critical
end program