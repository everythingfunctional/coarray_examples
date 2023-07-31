program hello_images
    implicit none

    critical
        print *, "Hello from image ", this_image(), " of ", num_images()
    end critical
end program
