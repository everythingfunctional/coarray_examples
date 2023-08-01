program hello_send_receive
    use communicator, only: communicator_t

    implicit none

    type(communicator_t) :: comm
    character(len=20) :: message[*]
    integer :: me, ni
    class(*), allocatable :: payload

    call comm%init()
    me = this_image()
    ni = num_images()
    if (me == ni) then
        write(message, "(A,I0)") "Hello from image ", me
    else
        call comm%receive_from(me+1, payload)
        select type (payload)
        type is (character(len=*))
            message = payload
        class default
            message = "Didn't get a string message"
        end select
    end if
    if (me > 1) call comm%send_to(me-1, message)
    critical
        print *, "Received message '" // trim(message) // "' on image ", me
    end critical
end program