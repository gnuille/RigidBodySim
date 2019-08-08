module physics_m
      use constants_m
      use vector2_m
      implicit none
      contains
              function pos_mrua( po, a, v, dt) result(p)
                        implicit none
                        type(vector2_t), intent(in):: v, a, po
                        type(vector2_t) :: p, aux
                        real(kind=dp), intent(in) :: dt

                        p = po
                        aux = scalar_product_v2( a, 0.5*dt*dt )
                        p = add_v2(p, aux)
                        aux = scalar_product_v2( v, dt)
                        p = add_v2(p, aux)
              end function pos_mrua

              function vel_mrua( vo, a, dt) result(v)
                      implicit none
                      type(vector2_t), intent(in) :: vo, a
                      type(vector2_t) :: v, aux
                      real(kind=dp), intent(in) :: dt

                      aux = scalar_product_v2(a, dt)
                      v = add_v2(vo, aux)
              end function vel_mrua

end module physics_m

