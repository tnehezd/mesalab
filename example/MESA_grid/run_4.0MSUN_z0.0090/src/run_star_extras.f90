! *****************************************************
!
!   run_star_extras file for Astero Across the HRD Labs
!
! *****************************************************

      module run_star_extras

      use star_lib
      use star_def
      use const_def
      use math_lib

  ! >>> Insert additional use statements below
      use gyre_lib

      implicit none

      !! GYRE stuff
!      real(dp), allocatable, save :: frequencies(:,:)
!      real(dp), allocatable, save :: etas(:)
!      real(dp), allocatable, save :: freq(:)
      real(dp), allocatable, save :: dataallin(:,:)
      ! Radial displacement eigenfunctions
!      real(dp), allocatable, save :: xi_r_radial(:)
      ! real(dp), allocatable, save :: xi_r_dipole(:)
      logical, save :: gyre_has_run

      integer :: stop_phase

      real(dp) :: obs_Teff_min, obs_Teff_max, obs_L_min, obs_L_max

      real(dp), dimension(40) :: prev_frequencies
      real(dp), dimension(40) :: current_frequencies
      real(dp), dimension(40) :: matched_frequencies
      integer, parameter :: max_modes = 40

      logical :: first_step = .true.  ! Logical for the first step! --- this is essential for checking frequencies

      ! these routines are called by the standard run_star check_model
      contains

      subroutine extras_controls(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return


         s% extras_startup => extras_startup
         s% extras_start_step => extras_start_step
         s% extras_check_model => extras_check_model
         s% extras_finish_step => extras_finish_step
         s% extras_after_evolve => extras_after_evolve
         s% how_many_extra_history_columns => how_many_extra_history_columns
         s% data_for_extra_history_columns => data_for_extra_history_columns
         s% how_many_extra_profile_columns => how_many_extra_profile_columns
         s% data_for_extra_profile_columns => data_for_extra_profile_columns

         s% how_many_extra_history_header_items => how_many_extra_history_header_items
         s% data_for_extra_history_header_items => data_for_extra_history_header_items
         s% how_many_extra_profile_header_items => how_many_extra_profile_header_items
         s% data_for_extra_profile_header_items => data_for_extra_profile_header_items

      end subroutine extras_controls


      subroutine extras_startup(id, restart, ierr)
         integer, intent(in) :: id
         logical, intent(in) :: restart
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return



         ! Initialize GYRE

         call gyre_init('gyre_non-ad.in')

         ! Set constants

         call gyre_set_constant('G_GRAVITY', standard_cgrav)
         call gyre_set_constant('C_LIGHT', clight)
         call gyre_set_constant('A_RADIATION', crad)

         call gyre_set_constant('M_SUN', Msun)
         call gyre_set_constant('R_SUN', Rsun)
         call gyre_set_constant('L_SUN', Lsun)

         call gyre_set_constant('GYRE_DIR', TRIM(mesa_dir)//'/gyre/gyre')

         !! changed all from 50 (from Dora's project) to 10

!         allocate(prev_frequencies(40))
!         allocate(current_frequencies(40))
!         allocate(matched_frequencies(40))
!         allocate(etas(40))
!         allocate(freq(40))
!         allocate(frequencies(2,40))
         allocate(dataallin(3,40))

      end subroutine extras_startup



      integer function extras_start_step(id)
         integer, intent(in) :: id
         integer :: ierr
         integer :: k !, nz
         type (star_info), pointer :: s !! don't need to define sub-components of this; knowing about "s" means it knows about all its entities
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         gyre_has_run = .false.

         do k = 1, 40 !50
!            frequencies(1,k) = 0   !! l = 0 ! radial
!            frequencies(2,k) = 0   !! l = 1 ! dipole
            dataallin(1,k) = 0
            dataallin(2,k) = 0
            dataallin(3,k) = 0
!            prev_frequencies = 0.0
!            current_frequencies = 0.0
!            matched_frequencies = 0.0

            !! initialize array for etas and freq
!            etas(k) = 0
!            freq(k) = 0
         end do



        !! insert timestep thing here? 6/2/23
        !! maybe better to try this with luminosity band first
        !
        ! type(mode_t), intent(in) :: md

        !! star data structure is defined here: /home/mjoyce/MESA/mesa-r23051/star_data/public/star_data_def.inc

        obs_Teff_min = s% x_ctrl(3)
        obs_Teff_max = s% x_ctrl(4)
        obs_L_min = s% x_ctrl(5)
        obs_L_max = s% x_ctrl(6)

        if (  (s% Teff >= obs_Teff_min) .and. &
              (s% Teff <= obs_Teff_max) .and. &
              (s% L( s% nz ) >= obs_L_min) .and. &
              (s% L( s% nz) <= obs_L_max) ) then

            write(*,*) 'model within the observational bounds; reducing timestep to 10 years...'
            s%max_years_for_timestep = 10
        endif


         extras_start_step = 0
      end function extras_start_step


      ! returns either keep_going, retry, or terminate.
      integer function extras_check_model(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         extras_check_model = keep_going
         if (.false. .and. s% star_mass_h1 < 0.35d0) then
            ! stop when star hydrogen mass drops to specified level
            extras_check_model = terminate
            write(*, *) 'have reached desired hydrogen mass'
            return
         end if

         if (s%x_logical_ctrl(1)) then
             print *, '... init gyre for run ...'

             call run_gyre(id, ierr)
         endif

         if (extras_check_model == terminate) s% termination_code = t_extras_check_model

      end function extras_check_model


      integer function how_many_extra_history_columns(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

!         how_many_extra_history_columns = 160 !10 !100 !! adding in outer cell gamma1
         how_many_extra_history_columns = 120 !10 !100 !! adding in outer cell gamma1
      end function how_many_extra_history_columns



      subroutine data_for_extra_history_columns(id, n, names, vals, ierr)
         integer, intent(in) :: id, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         integer :: phase_of_evolution
         integer :: k
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         !! GYRE stuff
         do k = 1, 40
            write (names(k),    '(A,I0)') 'radial_mode_', k-1
            write (names(k+40),    '(A,I0)') 'nu_radial_', k-1
            write (names(k+80), '(A,I0)') 'eta_radial_', k-1
!            write (names(k+120), '(A,I0)') 'matched_nu_', k-1
        end do

         !! remove dipoles; not necessary and causes crash on the RGB
         if (s%x_logical_ctrl(1)) then
            ! save the frequencies of the radial and dipole modes
           !! ensure that GYRE is being run at every timestep; tried commenting this out 6/2/23
           if (.NOT. gyre_has_run) then
              write(*,*) 'calling run_gyre'
!              print *, '... gyre is running 2x ...'
               call run_gyre(id, ierr)
           endif

            ! save the frequencies of the radial (not dipole) modes
            do k = 1, 40
            !    vals(k)    = frequencies(1, k)     ! frequencies for l=1 only (?) -- so this was a normalized frequency in uHz for Earl's minilab3
            !   vals(k)    = freq(k)          ! the real part of the frequency -- in general it's a complex number
               vals(k) = dataallin(1, k)
               vals(k+40) = dataallin(2,k)
               vals(k+80) = dataallin(3,k)
!               vals(k+120) = matched_frequencies(k)
            end do

         else
            ! write out zeros for the 2*50 columns
            do k = 1, 40 !100
                vals(k) = 0
                vals(k+40) = 0
                vals(k+80) = 0

            end do

         endif



      end subroutine data_for_extra_history_columns



      integer function how_many_extra_profile_columns(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_profile_columns = 0
      end function how_many_extra_profile_columns



      subroutine data_for_extra_profile_columns(id, n, nz, names, vals, ierr)
         integer, intent(in) :: id, n, nz
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(nz,n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         integer :: k
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return


      end subroutine data_for_extra_profile_columns


      integer function how_many_extra_history_header_items(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_history_header_items = 0
      end function how_many_extra_history_header_items


      subroutine data_for_extra_history_header_items(id, n, names, vals, ierr)
         integer, intent(in) :: id, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         type(star_info), pointer :: s
         integer, intent(out) :: ierr
         ierr = 0
         call star_ptr(id,s,ierr)
         if(ierr/=0) return
      end subroutine data_for_extra_history_header_items


      integer function how_many_extra_profile_header_items(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_profile_header_items = 0
      end function how_many_extra_profile_header_items


      subroutine data_for_extra_profile_header_items(id, n, names, vals, ierr)
         integer, intent(in) :: id, n
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(n)
         type(star_info), pointer :: s
         integer, intent(out) :: ierr
         ierr = 0
         call star_ptr(id,s,ierr)
         if(ierr/=0) return
      end subroutine data_for_extra_profile_header_items



      integer function extras_finish_step(id)
         integer, intent(in) :: id
         integer :: ierr, phase_of_evolution

         integer :: k, best_k, stop_phase
         real(dp) :: best_freq

         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         ! stop_phase = s% x_integer_ctrl(2)

         !  !! grab straight from star data structure
         !  !! 10 means TP_AGB? what are the definitions?
         !  !! !phase_of_evolution ! Integer mapping to the type of evolution see star_data/public/star_data_def.inc for definitions

         !  if (s% phase_of_evolution == stop_phase) then
         !       extras_finish_step = terminate
         !     write(*,*) 'terminate should be being set here, s%phase_of_evolution = ', s% phase_of_evolution
         !  else
         !     extras_finish_step = keep_going
         !  end if

         extras_finish_step = keep_going

         if (extras_finish_step == terminate) s% termination_code = t_extras_finish_step

      end function extras_finish_step


      subroutine extras_after_evolve(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
      end subroutine extras_after_evolve



      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
      ! a subroutine for checking pulsation modes and compares
      ! them with the previous step for not to make jumps in data
      ! 10/february/2025 -- TND
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine track_modes(prev_frequencies, current_frequencies, matched_frequencies)
          real(8), intent(in) :: prev_frequencies(40)  ! Az előző lépésbeli frekvenciák (40 elemű tömb)
          real(8), intent(inout) :: current_frequencies(40)  ! Az aktuális lépésbeli frekvenciák (40 elemű tömb)
          real(8), intent(out) :: matched_frequencies(40)  ! Az eredményül kapott, sorrendben rendezett frekvenciák (40 elemű tömb)

          integer :: i, j
          real(8) :: temp(40)  ! Ideiglenes tömb, hogy megőrizzük a current_frequencies értékeit
          real(8) :: diff, min_diff, threshold
          integer :: min_index

          ! Inicializáljuk a matched_frequencies tömböt nullákkal
          matched_frequencies = 0.0

          ! Hozzunk létre egy ideiglenes tömböt, hogy megőrizzük a current_frequencies értékeit
          temp = current_frequencies

          ! A prev_frequencies sorrendjében rendezzük a current_frequencies tömböt
          do i = 1, 40
              ! Küszöb meghatározása az ugrások detektálására
              threshold = max(0.00000005D0, 0.00000005D0 * abs(prev_frequencies(i)))
              ! Ha az előző lépésben nulla volt és most is nulla, akkor megtartjuk nullának
              if (prev_frequencies(i) == 0.0 .and. current_frequencies(i) == 0.0) then
                  matched_frequencies(i) = 0.0
              else
                  ! Ha az előző lépésben nem nulla volt, akkor keressük a legjobb illeszkedést
                  if (prev_frequencies(i) /= 0.0) then
                      min_diff = 1.0D30  ! Kezdetben egy nagyon nagy értékkel kezdjük
                      min_index = -1

                      ! Keresünk egy elemet a temp tömbben, ami a legjobban egyezik az előző lépésbeli értékkel
                      do j = 1, 40
                          diff = abs(prev_frequencies(i) - temp(j))
                          if (diff < min_diff .and. temp(j) /= 0.0) then
                              min_diff = diff
                              min_index = j
                          end if
                      end do

                      ! Ha találtunk egy legjobban illeszkedő elemet, azt helyezzük a matched_frequencies megfelelő indexére
                      if (min_index /= -1) then
                          ! Ugrás detektálása: ha hirtelen nullára esik, de az előző érték nagy volt, akkor inkább ne engedjük meg
                          if (prev_frequencies(i) /= 0.0 .and. temp(min_index) == 0.0 .and. abs(prev_frequencies(i)) > threshold) then
                              matched_frequencies(i) = prev_frequencies(i)  ! Megőrizzük a régi értéket
                          else
                              matched_frequencies(i) = temp(min_index)  ! Elfogadjuk az új értéket
                              temp(min_index) = 0.0  ! Nullázzuk ki, hogy ne találjuk meg újra
                          end if
                      end if
                  else
                      ! Ha az előző lépésbeli érték nulla volt, akkor az aktuális frekvenciát átadjuk, kivéve ha nagy ugrás történt
                      if (current_frequencies(i) > threshold) then
                          matched_frequencies(i) = 0.0  ! Ha nulláról hirtelen nagy értékre ugrott, inkább elutasítjuk
                      else
                          matched_frequencies(i) = current_frequencies(i)
                      end if
                  end if
              end if
          end do
      end subroutine track_modes




      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
      ! GYRE-on-the-fly
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine run_gyre (id, ierr)

         integer, intent(in)  :: id
         integer, intent(out) :: ierr


         integer :: i, j
         real(dp), allocatable :: global_data(:)
         real(dp), allocatable :: point_data(:,:)
         integer               :: ipar(0)
         real(dp)              :: rpar(0)

         ! Pass model data to GYRE

         call star_get_pulse_data(id, 'GYRE', .FALSE., .TRUE., .TRUE., &
              global_data, point_data, ierr)
         if (ierr /= 0) then
            print *,'Failed when calling star_get_pulse_data'
            return
         end if

         current_frequencies = 0.0

         if(first_step) then
            prev_frequencies = 0.0
         end if

         call gyre_set_model(global_data, point_data, 101)

         ! Run GYRE to get modes
         call gyre_get_modes(0, process_mode, ipar, rpar)
!         call gyre_get_modes(1, process_mode, ipar, rpar)
         gyre_has_run = .true.
         first_step = .false.
!         do i = 1, 40
         ! Kiíratjuk az eredményeket
!           print *, "Előző lépésben:", prev_frequencies(i), "Eredeti current_frequencies:", current_frequencies(i), "Rendezett matched_frequencies:", matched_frequencies(i)
!         end do
!         call track_modes(prev_frequencies, current_frequencies, matched_frequencies)

!         print *, ' MEGTÖRTENT A CSERE ES A RENDEZES '

!         do i = 1, 40
         ! Kiíratjuk az eredményeket
!           print *, "Előző lépésben:", prev_frequencies(i), "Eredeti current_frequencies:", current_frequencies(i), "Rendezett matched_frequencies:", matched_frequencies(i)
!         end do
!         prev_frequencies = matched_frequencies


      contains

         subroutine process_mode (md, ipar, rpar, retcode)


            type(mode_t), intent(in) :: md
            !integer :: md
            integer, intent(inout)   :: ipar(:)
            real(dp), intent(inout)  :: rpar(:)
            integer, intent(out)     :: retcode
            integer :: k !, n


            integer, parameter :: max_modes = 40  ! Max 40 módus
            integer :: mode_count = 0             ! Számláló a betöltött módusokhoz


            type (star_info), pointer :: s
            ierr = 0
            call star_ptr(id, s, ierr)
            if (ierr /= 0) return

              ! Print out degree, radial order, mode inertia, and frequency
!            print *, 'Checking mode: l = ', md%l, ' n_p = ', md%n_p, 'eta =', md%eta(), ' freq = ', REAL(md%freq('CYC_PER_DAY'))

            if (md%n_p >= 1 .and. md%n_p <= 40 .and. (md%l == 0) ) then

!              if(first_step) then
!                current_frequencies(md%n_p) = REAL(md%freq('CYC_PER_DAY'))
!                prev_frequencies(md%n_p) = REAL(md%freq('CYC_PER_DAY'))
!                matched_frequencies(md%n_p) = REAL(md%freq('CYC_PER_DAY'))
!                current_frequencies(md%n_p) = REAL(md%freq('HZ'))
!                prev_frequencies(md%n_p) = REAL(md%freq('HZ'))
!                matched_frequencies(md%n_p) = REAL(md%freq('HZ'))
!              else
!                current_frequencies(md%n_p) = REAL(md%freq('CYC_PER_DAY'))
!                current_frequencies(md%n_p) = REAL(md%freq('HZ'))
!              end if

                !! eta should be nonzero only during the instibality strip
!                 print *, '  (1)  Found mode: l, n_p, n_g, E_norm, E, nu, eta, W, E/W=      ', &
!                     md%l, md%n_p, md%n_g, md%E_norm(), md%E(), REAL(md%freq('CYC_PER_DAY')),  &
!                     md%eta(), md% W(), md%E()/md% W()

!                     print *, '  (1)  Found mode: n_p, E_norm, E, nu, eta, W, dW/dx=      ', &
!                        md%n_p, md%E_norm(), md%E(), REAL(md%freq('CYC_PER_DAY')),  &
!                         md%eta(), md% W(), md% dW_dx(md%n_p)


                ! if (md%l == 0) then ! radial modes
                !     print *, '  (2)  Found mode: l, n_p, n_g, E, nu, diff_nu, eta = ', &
                !     md%l, md%n_p, md%n_g, md%E_norm(), REAL(md%freq('HZ')), &
                !     (md%freq('UHZ') - s% nu_max) / s% delta_nu !, &
                !   !  md%eta()

                ! print*, '---------------'

!                    frequencies(md%l+1, md%n_p) = md% freq('HZ') !(md%freq('UHZ') - s% nu_max) / s% delta_nu
!                    frequencies(md%l+1, md%n_p) = md% freq('CYC_PER_DAY') !(md%freq('UHZ') - s% nu_max) / s% delta_nu
!                    etas(md%l+1) = md% eta()
!                    etas(md%l+1) = -99

!                    print *, '  (2)  Found mode: n_p, E_norm, E, nu, eta, W, dW/dx=      ', &
!                       md%n_p, md%E_norm(), md%E(), REAL(md%freq('CYC_PER_DAY')),  &
!                        etas(md%l+1), md% W(), md% dW_dx(md%n_p)

                        ! Mivel most azt akarjuk, hogy minden híváskor a dataallin tömböt frissítsük,
                        ! a következő módon töltjük fel azt:
                        dataallin(1, md%n_p) = md%n_p  ! Frequencies a 1. dimenzióban
!                        dataallin(2, md%n_p) = REAL(md%freq('CYC_PER_DAY'))
                        dataallin(2, md%n_p) = REAL(md%freq('HZ'))

                        dataallin(3, md%n_p) = md%eta()                ! Eta érték

                        ! Printeljük ki a dataallin tartalmát, hogy lássuk a frissített adatokat
!                        print *, "dataallin a végén: ", dataallin(1, md%l+1, md%n_p), dataallin(2, md%l+1, md%n_p), dataallin(3, md%l+1, md%n_p)

            end if

            ! Kiíratjuk a dataallin értékeket az if blokkon kívül
!            print *, "dataallin a végén: "
!            print *, "n_p: ", dataallin(1,:)
!            print *, "freq: ", dataallin(2,:)
!            print *, "Eta: ", dataallin(3,:)

            retcode = 0
         end subroutine process_mode
      end subroutine run_gyre




      end module run_star_extras
