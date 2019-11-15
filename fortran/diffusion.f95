! Translation of my C version.
! Fortran contains no NEAT LANGUAGE THINGs because fortran sucks.

! TESTING RESULTS:
! Without Partition:
! At Rdivs = 10, equilibriated in 51.438s simulated time, 4.873s real time.
! At Rdivs = 500, completes a single step in 22.127s.
! (Slower than C. Maybe due to the true 3d array?)

program diffusion
    !declarations
    implicit none
    !constants
    integer, parameter :: Rdivs = 10
    real(kind=8), parameter :: Lroom = 5.0, Urms = 250.0, D = 0.175
    !calculated constants
    real(kind=8) :: Rdist, Tstep, Dterm
    integer :: volelCount
    !variables
    integer :: xi, yi, zi
    real(kind=8) :: Ttotal
    !room, initialized to zero
    real(kind=8), dimension(Rdivs, Rdivs, Rdivs) :: room = 0.0

    !program
    !calculated initializations
    Rdist = Lroom/Rdivs
    Tstep = Lroom/Urms/Rdivs
    Dterm = D * Tstep / Rdist / Rdist
    volelCount = Rdivs*Rdivs*Rdivs
    Ttotal = 0.0
    
    !simulation
    room(1, 1, 1) = 1E21
    do while (minMaxRatio(room) .lt. 0.99)
        do zi = 1, Rdivs
        do yi = 1, Rdivs
        do xi = 1, Rdivs
            call diffuse(room, xi, yi, zi,-1, 0, 0)
            call diffuse(room, xi, yi, zi,+1, 0, 0)
            call diffuse(room, xi, yi, zi, 0,-1, 0)
            call diffuse(room, xi, yi, zi, 0,+1, 0)
            call diffuse(room, xi, yi, zi, 0, 0,-1)
            call diffuse(room, xi, yi, zi, 0, 0,+1)
        end do
        end do
        end do
        Ttotal = Ttotal + Tstep
    end do
    print "(F10.6,A,F9.8)", Ttotal, achar(9), minMaxRatio(room)

contains

function minMaxRatio(cube) result(ratio)
    implicit none
    real(kind=8), dimension(Rdivs,Rdivs,Rdivs), intent(in) :: cube
    real(kind=8) :: ratio
    real(kind=8) :: min, max
    integer :: x, y, z
    max = 0.0
    min = 1E100
    do x=1,Rdivs
        do y=1,Rdivs
            do z=1,Rdivs
                if (cube(x, y, z) .lt. min) then
                    min = cube(x, y, z)
                endif
                if (cube(x, y, z) .gt. max) then
                    max = cube(x, y, z)
                endif
            end do
        end do
    end do
    ratio = min/max
end function minMaxRatio

!performs diffusion between two volels. dx, dy, dz are offsets.
subroutine diffuse(room, x, y, z, dx, dy, dz)
    implicit none
    real(kind=8), dimension(Rdivs,Rdivs,Rdivs) :: room
    real(kind=8) :: change
    integer, intent(in) :: x, y, z, dx, dy, dz
    integer :: nx, ny, nz !can't modify d* in fortran so have to do this
    nx = x+dx
    ny = y+dy
    nz = z+dz
    !bound checking
    if (nx < 1 .or. nx > Rdivs .or. ny < 1 .or. ny > Rdivs .or. nz < 1 .or. &
    nz > Rdivs) then
        ! OK i lied, the one neat thing about fortran is that it has goto
        goto 100
    endif
    !diffusion
    change = (room(x, y, z) - room(nx, ny, nz))*Dterm
    room(x, y, z) = room(x, y, z) - change
    room(nx, ny, nz) = room(nx, ny, nz) + change
    100 continue
end subroutine diffuse

end program diffusion
