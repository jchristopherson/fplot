      subroutine alpha_measure ( n, z, triangle_order, triangle_num, 
     &  triangle_node, alpha_min, alpha_ave, alpha_area )

c*********************************************************************72
c
cc ALPHA_MEASURE determines the triangulated pointset quality measure ALPHA.
c
c  Discusion:
c
c    The ALPHA measure evaluates the uniformity of the shapes of the triangles
c    defined by a triangulated pointset.
c
c    We compute the minimum angle among all the triangles in the triangulated
c    dataset and divide by the maximum possible value (which, in degrees,
c    is 60).  The best possible value is 1, and the worst 0.  A good
c    triangulation should have an ALPHA score close to 1.
c
c    The code has been modified to 'allow' 6-node triangulations.
c    However, no effort is made to actually process the midside nodes.
c    Only information from the vertices is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision Z(2,N), the points.
c
c    Input, integer TRIANGLE_ORDER, the order of the triangles.
c
c    Input, integer TRIANGLE_NUM, the number of triangles.
c
c    Input, integer TRIANGLE_NODE(TRIANGLE_ORDER,TRIANGLE_NUM), 
c    the triangulation.
c
c    Output, double precision ALPHA_MIN, the minimum value of ALPHA over all
c    triangles.
c
c    Output, double precision ALPHA_AVE, the value of ALPHA averaged over
c    all triangles.
c
c    Output, double precision ALPHA_AREA, the value of ALPHA averaged over
c    all triangles and weighted by area.
c
      implicit none

      integer n
      integer triangle_num
      integer triangle_order

      double precision a_angle
      integer a_index
      double precision a_x
      double precision a_y
      double precision ab_len
      double precision alpha
      double precision alpha_area
      double precision alpha_ave
      double precision alpha_min
      double precision area
      double precision area_total
      double precision b_angle
      integer b_index
      double precision b_x
      double precision b_y
      double precision bc_len
      double precision c_angle
      integer c_index
      double precision c_x
      double precision c_y
      double precision ca_len
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_acos
      double precision r8_huge
      integer triangle
      integer triangle_node(triangle_order,triangle_num)
      double precision z(2,n)

      alpha_min = r8_huge ( )
      alpha_ave = 0.0D+00
      alpha_area = 0.0D+00
      area_total = 0.0D+00

      do triangle = 1, triangle_num

        a_index = triangle_node(1,triangle)
        b_index = triangle_node(2,triangle)
        c_index = triangle_node(3,triangle)

        a_x = z(1,a_index)
        a_y = z(2,a_index)
        b_x = z(1,b_index)
        b_y = z(2,b_index)
        c_x = z(1,c_index)
        c_y = z(2,c_index)

        area = 0.5D+00 * abs ( a_x * ( b_y - c_y ) 
     &                       + b_x * ( c_y - a_y ) 
     &                       + c_x * ( a_y - b_y ) )

        ab_len = sqrt ( ( a_x - b_x )**2 + ( a_y - b_y )**2 )
        bc_len = sqrt ( ( b_x - c_x )**2 + ( b_y - c_y )**2 )
        ca_len = sqrt ( ( c_x - a_x )**2 + ( c_y - a_y )**2 )
c
c  Take care of a ridiculous special case.
c
        if ( ab_len .eq. 0.0D+00 .and. 
     &       bc_len .eq. 0.0D+00 .and. 
     &       ca_len .eq. 0.0D+00 ) then

          a_angle = 2.0D+00 * pi / 3.0D+00
          b_angle = 2.0D+00 * pi / 3.0D+00
          c_angle = 2.0D+00 * pi / 3.0D+00

        else

          if ( ca_len .eq. 0.0D+00 .or. ab_len .eq. 0.0D+00 ) then
            a_angle = pi
          else
            a_angle = 
     &        r8_acos ( ( ca_len**2 + ab_len**2 - bc_len**2 ) 
     &        / ( 2.0D+00 * ca_len * ab_len ) )
          end if

          if ( ab_len .eq. 0.0D+00 .or. bc_len .eq. 0.0D+00 ) then
            b_angle = pi
          else
            b_angle = 
     &        r8_acos ( ( ab_len**2 + bc_len**2 - ca_len**2 ) 
     &        / ( 2.0D+00 * ab_len * bc_len ) )
          end if

          if ( bc_len .eq. 0.0D+00 .or. ca_len .eq. 0.0D+00 ) then
            c_angle = pi
          else
            c_angle = 
     &        r8_acos ( ( bc_len**2 + ca_len**2 - ab_len**2 ) 
     &        / ( 2.0D+00 * bc_len * ca_len ) )
          end if

        end if

        alpha_min = min ( alpha_min, a_angle )
        alpha_min = min ( alpha_min, b_angle )
        alpha_min = min ( alpha_min, c_angle )

        alpha_ave = alpha_ave + alpha_min

        alpha_area = alpha_area + area * alpha_min

        area_total = area_total + area

      end do

      alpha_ave = alpha_ave / dble ( triangle_num )
      alpha_area = alpha_area / area_total
c
c  Normalize angles from [0,pi/3] degrees into qualities in [0,1].
c
      alpha_min = alpha_min * 3.0D+00 / pi
      alpha_ave = alpha_ave * 3.0D+00 / pi
      alpha_area = alpha_area * 3.0D+00 / pi

      return
      end
      function angle_rad_2d ( p1, p2, p3 )

c*********************************************************************72
c
cc ANGLE_RAD_2D returns the angle swept out between two rays in 2D.
c
c  Discussion:
c
c    Except for the zero angle case, it should be true that
c
c      ANGLE_RAD_2D ( P1, P2, P3 ) + ANGLE_RAD_2D ( P3, P2, P1 ) = 2 * PI
c
c        P1
c        /
c       /
c      /
c     /
c    P2--------->P3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision P1(2), P2(2), P3(2), define the rays
c    P1 - P2 and P3 - P2 which define the angle.
c
c    Output, double precision ANGLE_RAD_2D, the angle swept out by the rays,
c    in radians.  0 <= ANGLE_RAD_2D < 2 * PI.  If either ray has zero
c    length, then ANGLE_RAD_2D is set to 0.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )

      double precision angle_rad_2d
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision p(dim_num)
      double precision p1(dim_num)
      double precision p2(dim_num)
      double precision p3(dim_num)

      p(1) = ( p3(1) - p2(1) ) * ( p1(1) - p2(1) ) 
     &     + ( p3(2) - p2(2) ) * ( p1(2) - p2(2) )


      p(2) = ( p3(1) - p2(1) ) * ( p1(2) - p2(2) ) 
     &     - ( p3(2) - p2(2) ) * ( p1(1) - p2(1) )

      if ( p(1) .eq. 0.0D+00 .and. p(2) .eq. 0.0D+00 ) then
        angle_rad_2d = 0.0D+00
        return
      end if

      angle_rad_2d = atan2 ( p(2), p(1) )

      if ( angle_rad_2d .lt. 0.0D+00 ) then
        angle_rad_2d = angle_rad_2d + 2.0D+00 * pi
      end if

      return
      end
      function diaedg ( x0, y0, x1, y1, x2, y2, x3, y3 )

c*****************************************************************************80
c
cc DIAEDG chooses a diagonal edge.
c
c  Discussion:
c
c    The routine determines whether 0--2 or 1--3 is the diagonal edge
c    that should be chosen, based on the circumcircle criterion, where
c    (X0,Y0), (X1,Y1), (X2,Y2), (X3,Y3) are the vertices of a simple
c    quadrilateral in counterclockwise order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2009
c
c  Author:
c
c    Original FORTRAN77 version by Barry Joe.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Barry Joe,
c    GEOMPACK - a software package for the generation of meshes
c    using geometric algorithms,
c    Advances in Engineering Software,
c    Volume 13, pages 325-331, 1991.
c
c  Parameters:
c
c    Input, double precision X0, Y0, X1, Y1, X2, Y2, X3, Y3, the
c    coordinates of the vertices of a quadrilateral, given in
c    counter clockwise order.
c
c    Output, integer DIAEDG, chooses a diagonal:
c    +1, if diagonal edge 02 is chosen;
c    -1, if diagonal edge 13 is chosen;
c     0, if the four vertices are cocircular.
c
      implicit none

      double precision ca
      double precision cb
      integer diaedg
      double precision dx10
      double precision dx12
      double precision dx30
      double precision dx32
      double precision dy10
      double precision dy12
      double precision dy30
      double precision dy32
      double precision r8_epsilon
      double precision s
      double precision tol
      double precision tola
      double precision tolb
      double precision x0
      double precision x1
      double precision x2
      double precision x3
      double precision y0
      double precision y1
      double precision y2
      double precision y3

      tol = 100.0D+00 * r8_epsilon ( )

      dx10 = x1 - x0
      dy10 = y1 - y0
      dx12 = x1 - x2
      dy12 = y1 - y2
      dx30 = x3 - x0
      dy30 = y3 - y0
      dx32 = x3 - x2
      dy32 = y3 - y2

      tola = tol * max ( abs ( dx10 ), abs ( dy10 ), 
     &  abs ( dx30 ), abs ( dy30 ) )

      tolb = tol * max ( abs ( dx12 ), abs ( dy12 ), 
     &  abs ( dx32 ), abs ( dy32 ) )

      ca = dx10 * dx30 + dy10 * dy30
      cb = dx12 * dx32 + dy12 * dy32

      if ( tola .lt. ca .and. tolb .lt. cb ) then

        diaedg = - 1

      else if ( ca .lt. - tola .and. cb .lt. - tolb ) then

        diaedg = 1

      else

        tola = max ( tola, tolb )
        s = ( dx10 * dy30 - dx30 * dy10 ) * cb 
     &    + ( dx32 * dy12 - dx12 * dy32 ) * ca

        if ( tola .lt. s ) then
          diaedg = - 1
        else if ( s .lt. - tola ) then
          diaedg = 1
        else
          diaedg = 0
        end if

      end if

      return
      end
      subroutine get_unit ( unit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If UNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, UNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    26 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer UNIT, the free unit number.
c
      implicit none

      integer i
      integer unit

      unit = 0

      do i = 1, 99

        if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

          open ( unit = i, err = 10, status = 'scratch' )
          close ( unit = i )

          unit = i

          return
        end if

10      continue

      end do

      return
      end
      function i4_modp ( i, j )

c*********************************************************************72
c
cc I4_MODP returns the nonnegative remainder of integer division.
c
c  Discussion:
c
c    If
c      NREM = I4_MODP ( I, J )
c      NMULT = ( I - NREM ) / J
c    then
c      I = J * NMULT + NREM
c    where NREM is always nonnegative.
c
c    The MOD function computes a result with the same sign as the
c    quantity being divided.  Thus, suppose you had an angle A,
c    and you wanted to ensure that it was between 0 and 360.
c    Then mod(A,360) would do, if A was positive, but if A
c    was negative, your result would be between -360 and 0.
c
c    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
c
c  Example:
c
c        I     J     MOD I4_MODP    Factorization
c
c      107    50       7       7    107 =  2 *  50 + 7
c      107   -50       7       7    107 = -2 * -50 + 7
c     -107    50      -7      43   -107 = -3 *  50 + 43
c     -107   -50      -7      43   -107 =  3 * -50 + 43
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the number to be divided.
c
c    Input, integer J, the number that divides I.
c
c    Output, integer I4_MODP, the nonnegative remainder when I is
c    divided by J.
c
      implicit none

      integer i
      integer i4_modp
      integer j
      integer value

      if ( j .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_MODP - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal divisor J = ', j
        stop
      end if

      value = mod ( i, j )

      if ( value .lt. 0 ) then
        value = value + abs ( j )
      end if

      i4_modp = value

      return
      end
      subroutine i4_swap ( i, j )

c*********************************************************************72
c
cc I4_SWAP switches two I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer I, J.  On output, the values of I and
c    J have been interchanged.
c
      implicit none

      integer i
      integer j
      integer k

      k = i
      i = j
      j = k

      return
      end
      function i4_wrap ( ival, ilo, ihi )

c*********************************************************************72
c
cc I4_WRAP forces an I4 to lie between given limits by wrapping.
c
c  Example:
c
c    ILO = 4, IHI = 8
c
c    I  Value
c
c    -2     8
c    -1     4
c     0     5
c     1     6
c     2     7
c     3     8
c     4     4
c     5     5
c     6     6
c     7     7
c     8     8
c     9     4
c    10     5
c    11     6
c    12     7
c    13     8
c    14     4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer IVAL, an integer value.
c
c    Input, integer ILO, IHI, the desired bounds for the integer value.
c
c    Output, integer I4_WRAP, a "wrapped" version of IVAL.
c
      implicit none

      integer i4_modp
      integer i4_wrap
      integer ihi
      integer ilo
      integer ival
      integer jhi
      integer jlo
      integer value
      integer wide

      jlo = min ( ilo, ihi )
      jhi = max ( ilo, ihi )

      wide = jhi - jlo + 1

      if ( wide .eq. 1 ) then
        value = jlo
      else
        value = jlo + i4_modp ( ival - jlo, wide )
      end if

      i4_wrap = value

      return
      end
      subroutine i4mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
c
c  Discussion:
c
c    An I4MAT is a rectangular array of integer values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    39 October 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), an M by N matrix to be printed.
c
c    Input, character * ( * ) TITLE, an optional title.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      character * ( * ) title

      call i4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine i4mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &   jhi, title )

c*********************************************************************72
c
cc I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
c
c  Discussion:
c
c    An I4MAT is a rectangular array of integer values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 October 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, an optional title.
c
      implicit none

      integer incx
      parameter ( incx = 10 )
      integer m
      integer n

      integer a(m,n)
      character*8 ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer  j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      if ( 0 .lt. len ( title ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' )  title
      end if

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8)' ) i
        end do

        write ( *, '(''  Row '',10a8)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Col'
        write ( *, '(a)' ) ' '

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc

            i = i2lo - 1 + i2

            write ( ctemp(i2), '(i8)' ) a(i,j)

          end do

          write ( *, '(i5,1x,10a8)' ) j, ( ctemp(i), i = 1, inc )

        end do

      end do

      return
      end
      subroutine i4vec_heap_d ( n, a )

c*********************************************************************72
c
cc I4VEC_HEAP_D reorders an I4VEC into an descending heap.
c
c  Discussion:
c
c    An I4VEC is a vector of I4 values.
c
c    A descending heap is an array A with the property that, for every index J,
c    A(J) >= A(2*J) and A(J) >= A(2*J+1), (as long as the indices
c    2*J and 2*J+1 are legal).
c
c                  A(1)
c                /      \
c            A(2)         A(3)
c          /     \        /  \
c      A(4)       A(5)  A(6) A(7)
c      /  \       /   \
c    A(8) A(9) A(10) A(11)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms,
c    Academic Press, 1978, second edition,
c    ISBN 0-12-519260-6.
c
c  Parameters:
c
c    Input, integer N, the size of the input array.
c
c    Input/output, integer A(N).
c    On input, an unsorted array.
c    On output, the array has been reordered into a heap.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer ifree
      integer key
      integer m
c
c  Only nodes N/2 down to 1 can be "parent" nodes.
c
      do i = n/2, 1, -1
c
c  Copy the value out of the parent node.
c  Position IFREE is now "open".
c
        key = a(i)
        ifree = i

10      continue
c
c  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
c  IFREE.  (One or both may not exist because they exceed N.)
c
          m = 2 * ifree
c
c  Does the first position exist?
c
          if ( n .lt. m ) then
            go to 20
          end if
c
c  Does the second position exist?
c
          if ( m + 1 .le. n ) then
c
c  If both positions exist, take the larger of the two values,
c  and update M if necessary.
c
            if ( a(m) .lt. a(m+1) ) then
              m = m + 1
            end if

          end if
c
c  If the large descendant is larger than KEY, move it up,
c  and update IFREE, the location of the free position, and
c  consider the descendants of THIS position.
c
          if ( a(m) .le. key ) then
            go to 20
          end if

          a(ifree) = a(m)
          ifree = m

        go to 10
c
c  Once there is no more shifting to do, KEY moves into the free spot IFREE.
c
20      continue

        a(ifree) = key

      end do

      return
      end
      subroutine i4vec_min ( n, a, amin )

c*********************************************************************72
c
cc I4VEC_MIN computes the minimum element of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer AMIN, the value of the smallest entry.
c
      implicit none

      integer n

      integer a(n)
      integer amin
      integer i

      amin = a(1)

      do i = 2, n
        amin = min ( amin, a(i) )
      end do

      return
      end
      subroutine i4vec_sort_heap_a ( n, a )

c*********************************************************************72
c
cc I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
c
c  Discussion:
c
c    An I4VEC is a vector of I4 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms,
c    Academic Press, 1978, second edition,
c    ISBN 0-12-519260-6.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input/output, integer A(N).
c    On input, the array to be sorted;
c    On output, the array has been sorted.
c
      implicit none

      integer n

      integer a(n)
      integer n1

      if ( n .le. 1 ) then
        return
      end if
c
c  1: Put A into descending heap form.
c
      call i4vec_heap_d ( n, a )
c
c  2: Sort A.
c
c  The largest object in the heap is in A(1).
c  Move it to position A(N).
c
      call i4_swap ( a(1), a(n) )
c
c  Consider the diminished heap of size N1.
c
      do n1 = n - 1, 2, -1
c
c  Restore the heap structure of A(1) through A(N1).
c
        call i4vec_heap_d ( n1, a )
c
c  Take the largest object from A(1) and move it to A(N1).
c
        call i4_swap ( a(1), a(n1) )

      end do

      return
      end
      subroutine i4vec_sorted_unique ( n, a, unique_num )

c*********************************************************************72
c
cc I4VEC_SORTED_UNIQUE finds the unique elements in a sorted I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of elements in A.
c
c    Input/output, integer A(N).  On input, the sorted
c    integer array.  On output, the unique elements in A.
c
c    Output, integer UNIQUE_NUM, the number of unique elements in A.
c
      implicit none

      integer n

      integer a(n)
      integer itest
      integer unique_num

      if ( n .le. 0 ) then
        unique_num = 0
        return
      end if

      unique_num = 1

      do itest = 2, n

        if ( a(itest) .ne. a(unique_num) ) then
          unique_num = unique_num + 1
          a(unique_num) = a(itest)
        end if

      end do

      return
      end
      function lrline ( xu, yu, xv1, yv1, xv2, yv2, dv )

c*********************************************************************72
c
cc LRLINE determines if a point is left of, right or, or on a directed line.
c
c  Discussion:
c
c    The directed line is parallel to, and at a signed distance DV from
c    a directed base line from (XV1,YV1) to (XV2,YV2).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2009
c
c  Author:
c
c    Original FORTRAN77 version by Barry Joe.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Barry Joe,
c    GEOMPACK - a software package for the generation of meshes
c    using geometric algorithms,
c    Advances in Engineering Software,
c    Volume 13, pages 325-331, 1991.
c
c  Parameters:
c
c    Input, double precision XU, YU, the coordinates of the point whose
c    position relative to the directed line is to be determined.
c
c    Input, double precision XV1, YV1, XV2, YV2, the coordinates of two points
c    that determine the directed base line.
c
c    Input, double precision DV, the signed distance of the directed line
c    from the directed base line through the points (XV1,YV1) and (XV2,YV2).
c    DV is positive for a line to the left of the base line.
c
c    Output, integer ( kind = 4 ) LRLINE, the result:
c    +1, the point is to the right of the directed line;
c     0, the point is on the directed line;
c    -1, the point is to the left of the directed line.
c
      implicit none

      double precision dv
      double precision dx
      double precision dxu
      double precision dy
      double precision dyu
      integer ( kind = 4 ) lrline
      double precision r8_epsilon
      double precision t
      double precision tol
      double precision tolabs
      double precision xu
      double precision xv1
      double precision xv2
      double precision yu
      double precision yv1
      double precision yv2

      tol = 100.0D+00 * r8_epsilon ( tol )

      dx = xv2 - xv1
      dy = yv2 - yv1
      dxu = xu - xv1
      dyu = yu - yv1

      tolabs = tol * max ( abs ( dx ), abs ( dy ), abs ( dxu ), 
     &  abs ( dyu ), abs ( dv ) )

      t = dy * dxu - dx * dyu + dv * sqrt ( dx * dx + dy * dy )

      if ( tolabs .lt. t ) then
        lrline = 1
      else if ( - tolabs .le. t ) then
        lrline = 0
      else
        lrline = -1
      end if

      return
      end
      subroutine perm_check2 ( n, p, base, ierror )

c*********************************************************************72
c
cc PERM_CHECK2 checks that a vector represents a permutation.
c
c  Discussion:
c
c    The routine verifies that each of the integers from BASE to
c    to BASE+N-1 occurs among the N entries of the permutation.
c
c    Set the input quantity BASE to 0, if P is a 0-based permutation,
c    or to 1 if P is a 1-based permutation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries.
c
c    Input, integer P(N), the array to check.
c
c    Input, integer BASE, the index base.
c
c    Output, integer IERROR, error flag.
c    0, the array represents a permutation.
c    nonzero, the array does not represent a permutation.  The smallest
c    missing value is equal to IERROR.
c
      implicit none

      integer n

      integer base
      integer find
      integer ierror
      integer p(n)
      integer seek

      ierror = 0

      do seek = base, base + n - 1

        ierror = 1

        do find = 1, n
          if ( p(find) .eq. seek ) then
            ierror = 0
            go to 10
          end if
        end do

10      continue

        if ( ierror .ne. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'PERM_CHECK2 - Fatal error!'
          write ( *, '(a)' ) '  The input array does not represent'
          write ( *, '(a)' ) '  a proper permutation.'
          stop
        end if

      end do

      return
      end
      subroutine perm_inverse ( n, p )

c*********************************************************************72
c
cc PERM_INVERSE inverts a permutation "in place".
c
c  Discussion:
c
c    This algorithm assumes that the entries in the permutation vector are
c    strictly positive.  In particular, the value 0 must not occur.
c
c    When necessary, this function shifts the data temporarily so that
c    this requirement is satisfied.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2009
c
c  Parameters:
c
c    Input, integer N, the number of objects being permuted.
c
c    Input/output, integer P(N), the permutation, in standard index form.
c    On output, P describes the inverse permutation
c
      implicit none

      integer n

      integer base
      integer i
      integer i0
      integer i1
      integer i2
      integer ierror
      integer is
      integer p(n)
      integer p_min

      if ( n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PERM_INVERSE - Fatal error!'
        write ( *, '(a,i8)' ) '  Input value of N = ', n
        stop
      end if
c
c  Find the least value, and shift data so it begins at 1.
c
      call i4vec_min ( n, p, p_min )
      base = 1
      do i = 1, n
        p(i) = p(i) - p_min + base
      end do
c
c  Check the permutation.
c
      call perm_check2 ( n, p, base, ierror )

      if ( ierror .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PERM_INVERSE - Fatal error!'
        write ( *, '(a)' ) '  PERM_CHECK rejects this permutation.'
        stop
      end if
c
c  Invert the permutation.
c
      is = 1

      do i = 1, n

        i1 = p(i)

10      continue

        if ( i .lt. i1 ) then
          i2 = p(i1)
          p(i1) = -i2
          i1 = i2
          go to 10
        end if

        is = -sign ( 1, p(i) )
        p(i) = sign ( p(i), is )

      end do

      do i = 1, n

        i1 = -p(i)

        if ( 0 .le. i1 ) then

          i0 = i

20        continue

            i2 = p(i1)
            p(i1) = i0

            if ( i2 .lt. 0 ) then
              go to 30
            end if

            i0 = i1
            i1 = i2

          go to 20

30        continue

        end if

      end do
c
c  Undo the shift.
c
      do i = 1, n
        p(i) = p(i) + p_min - base
      end do

      return
      end
      subroutine points_delaunay_naive_2d ( node_num, node_xy, maxtri, 
     &  triangle_num, triangle_node )

!*********************************************************************72
!
!! POINTS_DELAUNAY_NAIVE_2D is a naive Delaunay triangulation scheme.
!
!  Discussion:
!
!    This routine is only suitable as a demonstration code for small
!    problems.  Its running time is of order NODE_NUM**4.  Much faster
!    algorithms are available.
!
!    Given a set of nodes in the plane, a triangulation is set of
!    triples of distinct nodes, forming triangles, so that every
!    point within the convex hull of the set of nodes is either
!    one of the nodes, or lies on an edge of one or more triangles,
!    or lies within exactly one triangle.
!
!    A Delaunay triangulation is a triangulation with additional
!    properties.
!
!    NODE_NUM must be at least 3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 June 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Joseph ORourke,
!    Computational Geometry,
!    Cambridge University Press,
!    Second Edition, 1998, page 187.
!
!  Parameters:
!
!    Input, integer NODE_NUM, the number of nodes.  
!
!    Input, double precision NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Input, integer MAXTRI, the maximum number of triangles.
!
!    Output, integer TRIANGLE_NUM, the number of triangles in 
!    the triangulation.
!
!    Output, integer TRIANGLE_NODE(3,MAXTRI), the indices of 
!    the triangle nodes.
!
      implicit none

      integer, parameter :: dim_num = 2
      integer maxtri
      integer node_num

      logical flag
      integer i
      integer j
      integer k
      integer m
      double precision node_xy(dim_num,node_num)
      integer triangle_node(3,maxtri)
      integer triangle_num
      double precision xn
      double precision yn
      double precision z(node_num)
      double precision zn

      triangle_num = 0

      if ( node_num .lt. 3 ) then
        return
      end if
!
!  Compute Z = X*X + Y*Y.
!
      do i = 1, node_num
        z(i) = node_xy(1,i)**2 + node_xy(2,i)**2
      end do
!  
!  For each triple (I,J,K):
!
      do i = 1, node_num - 2
        do j = i + 1, node_num
          do k = i + 1, node_num

            if ( j .ne. k ) then

              xn = ( node_xy(2,j) - node_xy(2,i) ) * ( z(k) - z(i) ) 
     &           - ( node_xy(2,k) - node_xy(2,i) ) * ( z(j) - z(i) )

              yn = ( node_xy(1,k) - node_xy(1,i) ) * ( z(j) - z(i) ) 
     &           - ( node_xy(1,j) - node_xy(1,i) ) * ( z(k) - z(i) )

              zn = ( node_xy(1,j) - node_xy(1,i) ) 
     &           * ( node_xy(2,k) - node_xy(2,i) ) 
     &           - ( node_xy(1,k) - node_xy(1,i) ) 
     &           * ( node_xy(2,j) - node_xy(2,i) )

              flag = ( zn .lt. 0.0D+00 )

              if ( flag ) then
                do m = 1, node_num
                  flag = flag .and. 
     &              ( ( node_xy(1,m) - node_xy(1,i) ) * xn 
     &              + ( node_xy(2,m) - node_xy(2,i) ) * yn 
     &              + ( z(m)   - z(i) )   * zn .le. 0.0D+00 )
                end do
              end if

              if ( flag ) then
                if ( triangle_num .lt. maxtri ) then
                  triangle_num = triangle_num + 1
                  triangle_node(1,triangle_num) = i
                  triangle_node(2,triangle_num) = j
                  triangle_node(3,triangle_num) = k
                end if
              end if

            end if

          end do
        end do
      end do

      return
      end
      subroutine points_hull_2d ( node_num, node_xy, hull_num, hull )

c*********************************************************************72
c
cc POINTS_HULL_2D computes the convex hull of 2D points.
c
c  Discussion:
c
c    The work involved is N*log(H), where N is the number of points, and H is
c    the number of points that are on the hull.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XY(2,NODE_NUM), the coordinates of the nodes.
c
c    Output, integer HULL_NUM, the number of nodes that lie on 
c    the convex hull.
c
c    Output, integer HULL(NODE_NUM).  Entries 1 through HULL_NUM 
c    contain the indices of the nodes that form the convex hull, in order.
c
      implicit none

      integer node_num
      integer dim_num
      parameter ( dim_num = 2 )

      double precision angle
      double precision angle_max
      double precision angle_rad_2d
      double precision di
      double precision dr
      integer first
      integer hull(node_num)
      integer hull_num
      integer i
      double precision node_xy(dim_num,node_num)
      double precision p_xy(dim_num)
      integer q
      double precision q_xy(2)
      integer r
      double precision r_xy(2)

      if ( node_num .lt. 1 ) then
        hull_num = 0
        return
      end if
c
c  If NODE_NUM = 1, the hull is the point.
c
      if ( node_num .eq. 1 ) then
        hull_num = 1
        hull(1) = 1
        return
      end if
c
c  If NODE_NUM = 2, then the convex hull is either the two distinct points,
c  or possibly a single (repeated) point.
c
      if ( node_num .eq. 2 ) then

        if ( node_xy(1,1) .ne. node_xy(1,2) .or. 
     &       node_xy(2,1) .ne. node_xy(2,2) ) then
          hull_num = 2
          hull(1) = 1
          hull(2) = 2
        else
          hull_num = 1
          hull(1) = 1
        end if

        return

      end if
c
c  Find the leftmost point, and take the bottom-most in a tie.
c  Call it "Q".
c
      q = 1
      do i = 2, node_num
        if ( node_xy(1,i) .lt. node_xy(1,q) .or. 
     &    ( node_xy(1,i) .eq. node_xy(1,q) .and. 
     &      node_xy(2,i) .lt. node_xy(2,q) ) ) then
          q = i
        end if
      end do

      q_xy(1) = node_xy(1,q)
      q_xy(2) = node_xy(2,q)
c
c  Remember the starting point, so we know when to stop!
c
      first = q
      hull_num = 1
      hull(1) = q
c
c  For the first point, make a dummy previous point, 1 unit south,
c  and call it "P".
c
      p_xy(1) = q_xy(1)
      p_xy(2) = q_xy(2) - 1.0D+00
c
c  Now, having old point P, and current point Q, find the new point R
c  so the angle PQR is maximal.
c
c  Watch out for the possibility that the two nodes are identical.
c
10    continue

        r = 0
        angle_max = 0.0D+00

        do i = 1, node_num

          if ( i .ne. q .and.
     &      ( node_xy(1,i) .ne. q_xy(1) .or.
     &        node_xy(2,i) .ne. q_xy(2) ) ) then

            angle = angle_rad_2d ( p_xy, q_xy, node_xy(1,i) )

            if ( r .eq. 0 .or. angle_max .lt. angle ) then

              r = i
              r_xy(1) = node_xy(1,r)
              r_xy(2) = node_xy(2,r)
              angle_max = angle
c
c  In case of ties, choose the nearer point.
c
            else if ( r .ne. 0 .and. angle .eq. angle_max ) then

              di = ( node_xy(1,i) - q_xy(1) )**2 
     &           + ( node_xy(2,i) - q_xy(2) )**2

              dr = ( r_xy(1)      - q_xy(1) )**2 
     &           + ( r_xy(2)      - q_xy(2) )**2

              if ( di .lt. dr ) then
                r = i
                r_xy(1) = node_xy(1,r)
                r_xy(2) = node_xy(2,r)
                angle_max = angle
              end if

            end if

          end if

        end do
c
c  If we've returned to our starting point, exit.
c
        if ( r .eq. first ) then
          go to 20
        end if

        hull_num = hull_num + 1

        if ( node_num .lt. hull_num ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'POINTS_HULL_2D - Fatal error!'
          write ( *, '(a)' ) '  The algorithm failed.'
          stop
        end if
c
c  Add point R to the convex hull.
c
        hull(hull_num) = r
c
c  Set P = Q, Q = R, and prepare to search for the next point R.
c
        q = r

        p_xy(1) = q_xy(1)
        p_xy(2) = q_xy(2)

        q_xy(1) = r_xy(1)
        q_xy(2) = r_xy(2)

      go to 10

20    continue

      return
      end
      subroutine quad_convex_random ( seed, xy )

c*********************************************************************72
c
cc QUAD_CONVEX_RANDOM returns a random convex quadrilateral.
c
c  Description:
c
c    The quadrilateral is constrained in that the vertices must all lie
c    with the unit square.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Output, double precision XY(2,NODE_NUM), the coordinates of the 
c    nodes of the quadrilateral, given in counterclockwise order.
c
      implicit none

      integer node_num
      parameter ( node_num = 4 )

      integer i
      integer ival(node_num)
      integer j
      integer nval
      integer seed
      double precision xy(2,node_num)
      double precision xy_random(2,node_num)

10    continue
c
c  Generate 4 random points.
c
        call r8mat_uniform_01 ( 2, node_num, seed, xy_random )
c
c  Determine the convex hull.
c
        call points_hull_2d ( node_num, xy_random, nval, ival )
c
c  If NVAL .lt. NODE_NUM, then our convex hull is a triangle.
c  Try again.
c
        if ( nval .eq. node_num ) then
          go to 20
        end if

      go to 10
c
c  Make an ordered copy of the random points.
c
20    continue

      do j = 1, nval
        do i = 1, 2
          xy(i,j) = xy_random(i,ival(j))
        end do
      end do

      return
      end
      function r8_acos ( c )

c*********************************************************************72
c
cc R8_ACOS computes the arc cosine function, with argument truncation.
c
c  Discussion:
c
c    If you call your system ACOS routine with an input argument that is
c    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant
c    surprise (I did).
c
c    This routine simply truncates arguments outside the range.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 October 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision C, the argument.
c
c    Output, double precision R8_ACOS, an angle whose cosine is C.
c
      implicit none

      double precision c
      double precision c2
      double precision r8_acos

      c2 = c
      c2 = max ( c2, -1.0D+00 )
      c2 = min ( c2, +1.0D+00 )

      r8_acos = acos ( c2 )

      return
      end
      function r8_epsilon ( )

c*********************************************************************72
c
cc R8_EPSILON returns the R8 roundoff unit.
c
c  Discussion:
c
c    The roundoff unit is a number R which is a power of 2 with the
c    property that, to the precision of the computer's arithmetic,
c      1 .lt. 1 + R
c    but
c      1 = ( 1 + R / 2 )
c
c    FORTRAN90 provides the superior library routine
c
c      EPSILON ( X )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_EPSILON, the R8 roundoff unit.
c
      implicit none

      double precision r8_epsilon

      r8_epsilon = 2.220446049250313D-016

      return
      end
      function r8_huge ( )

c*********************************************************************72
c
cc R8_HUGE returns a "huge" R8.
c
c  Discussion:
c
c    The value returned by this function is NOT required to be the
c    maximum representable R8.  This value varies from machine to machine,
c    from compiler to compiler, and may cause problems when being printed.
c    We simply want a "very large" but non-infinite number.
c
c    FORTRAN90 provides a built-in routine HUGE ( X ) that
c    can return the maximum representable number of the same datatype
c    as X, if that is what is really desired.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_HUGE, a huge number.
c
      implicit none

      double precision r8_huge

      r8_huge = 1.0D+30

      return
      end
      subroutine r82vec_part_quick_a ( n, a, l, r )

c*********************************************************************72
c
cc R82VEC_PART_QUICK_A reorders an R82VEC as part of a quick sort.
c
c  Discussion:
c
c    An R82VEC is an array of pairs of R82 values.
c
c    The routine reorders the entries of A.  Using A(1:2,1) as a
c    key, all entries of A that are less than or equal to the key will
c    precede the key, which precedes all entries that are greater than the key.
c
c  Example:
c
c    Input:
c
c      N = 8
c
c      A = ( (2,4), (8,8), (6,2), (0,2), (10,6), (10,0), (0,6), (4,8) )
c
c    Output:
c
c      L = 2, R = 4
c
c      A = ( (0,2), (0,6), (2,4), (8,8), (6,2), (10,6), (10,0), (4,8) )
c             -----------          ----------------------------------
c             LEFT          KEY    RIGHT
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries of A.
c
c    Input/output, double precision A(2,N).  On input, the array to be checked.
c    On output, A has been reordered as described above.
c
c    Output, integer L, R, the indices of A that define the three
c    segments.  Let KEY = the input value of A(1:2,1).  Then
c    I <= L                 A(1:2,I) < KEY;
c         L < I < R         A(1:2,I) = KEY;
c                 R <= I    KEY < A(1:2,I).
c
      implicit none

      integer n
      integer dim_num
      parameter ( dim_num = 2 )

      double precision a(dim_num,n)
      integer i
      integer j
      double precision key(dim_num)
      integer l
      integer m
      integer r
      logical r8vec_eq
      logical r8vec_gt
      logical r8vec_lt

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R82VEC_PART_QUICK_A - Fatal errorc'
        write ( *, '(a)' ) '  N < 1.'
        write ( *, '(a,i8)' ) '  N = ', n
        stop
      else if ( n .eq. 1 ) then
        l = 0
        r = 2
        return
      end if

      do i = 1, dim_num
        key(i) = a(i,1)
      end do

      m = 1
c
c  The elements of unknown size have indices between L+1 and R-1.
c
      l = 1
      r = n + 1

      do i = 2, n

        if ( r8vec_gt ( dim_num, a(1,l+1), key ) ) then
          r = r - 1
          call r8vec_swap ( dim_num, a(1,r), a(1,l+1) )
        else if ( r8vec_eq ( dim_num, a(1,l+1), key ) ) then
          m = m + 1
          call r8vec_swap ( dim_num, a(1,m), a(1,l+1) )
          l = l + 1
        else if ( r8vec_lt ( dim_num, a(1,l+1), key ) ) then
          l = l + 1
        end if

      end do
c
c  Now shift small elements to the left, and KEY elements to center.
c
      do j = 1, l - m
        do i = 1, dim_num
          a(i,j) = a(i,j+m)
        end do
      end do

      l = l - m

      do j = l + 1, l + m
        do i = 1, dim_num
          a(i,j) = key(i)
        end do
      end do

      return
      end
      subroutine r82vec_permute ( n, p, a )

c*********************************************************************72
c
cc R82VEC_PERMUTE permutes an R82VEC in place.
c
c  Discussion:
c
c    An R82VEC is an array of pairs of R8 values.
c
c    The same logic can be used to permute an array of objects of any 
c    arithmetic type, or an array of objects of any complexity.  The only
c    temporary storage required is enough to store a single object.  The number
c    of data movements made is N + the number of cycles of order 2 or more,
c    which is never more than N + N/2.
c
c  Example:
c
c    Input:
c
c      N = 5
c      P = (   2,    4,    5,    1,    3 )
c      A = ( 1.0,  2.0,  3.0,  4.0,  5.0 )
c          (11.0, 22.0, 33.0, 44.0, 55.0 )
c
c    Output:
c
c      A    = (  2.0,  4.0,  5.0,  1.0,  3.0 )
c             ( 22.0, 44.0, 55.0, 11.0, 33.0 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of objects.
c
c    Input, integer P(N), the permutation.  P(I) = J means
c    that the I-th element of the output array should be the J-th
c    element of the input array.  
c
c    Input/output, double precision A(2,N), the array to be permuted.
c
      implicit none

      integer n
      integer, parameter :: dim_num = 2

      double precision a(dim_num,n)
      double precision a_temp(dim_num)
      integer base
      parameter ( base = 1 )
      integer dim
      integer ierror
      integer iget
      integer iput
      integer istart
      integer p(n)

      call perm_check2 ( n, p, base, ierror )

      if ( ierror .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R82VEC_PERMUTE - Fatal error!'
        write ( *, '(a)' ) '  PERM_CHECK rejects this permutation.'
        stop
      end if
c
c  Search for the next element of the permutation that has not been used.
c
      do istart = 1, n

        if ( p(istart) .lt. 0 ) then

        else if ( p(istart) .eq. istart ) then

          p(istart) = - p(istart)

        else

          do dim = 1, dim_num
            a_temp(dim) = a(dim,istart)
          end do
          iget = istart
c
c  Copy the new value into the vacated entry.
c
10        continue

            iput = iget
            iget = p(iget)

            p(iput) = - p(iput)

            if ( iget .lt. 1 .or. n .lt. iget ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'R82VEC_PERMUTE - Fatal error!'
              write ( *, '(a)' ) 
     &          '  A permutation index is out of range.'
              write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
              stop
            end if

            if ( iget .eq. istart ) then
              do dim = 1, dim_num
                a(dim,iput) = a_temp(dim)
              end do
              go to 20
            end if

            do dim = 1, dim_num
              a(dim,iput) = a(dim,iget)
            end do

          go to 10

        end if

20      continue

      end do
c
c  Restore the signs of the entries.
c
      do istart = 1, n
        p(istart) = - p(istart)
      end do

      return
      end
      subroutine r82vec_sort_heap_index_a ( n, a, indx )

c*********************************************************************72
c
cc R82VEC_SORT_HEAP_INDEX_A ascending index heaps an R82VEC.
c
c  Discussion:
c
c    An R82VEC is an array of R82's.
c
c    The sorting is not actually carried out.  Rather an index array is
c    created which defines the sorting.  This array may be used to sort
c    or index the array, or to sort or index related arrays keyed on the
c    original array.
c
c    Once the index array is computed, the sorting can be carried out
c    "implicitly:
c
c      A(1:2,INDX(1:N)) is sorted,
c
c    or explicitly, by the call
c
c      call r82vec_permute ( n, indx, a )
c
c    after which A(1:2,I), I = 1 to N is sorted.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(2,N), an array to be index-sorted.
c
c    Output, integer INDX(N), the sort index.  The
c    I-th element of the sorted array is A(1:2,INDX(I)).
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n

      double precision a(dim_num,n)
      double precision aval(dim_num)
      integer dim
      integer i
      integer indx(n)
      integer indxt
      integer ir
      integer j
      integer l

      if ( n .lt. 1 ) then
        return
      end if

      do i = 1, n
        indx(i) = i
      end do

      if ( n .eq. 1 ) then
        return
      end if

      l = n / 2 + 1
      ir = n

10    continue

        if ( 1 .lt. l ) then

          l = l - 1
          indxt = indx(l)
          do dim = 1, dim_num
            aval(dim) = a(dim,indxt)
          end do

        else

          indxt = indx(ir)
          do dim = 1, dim_num
            aval(dim) = a(dim,indxt)
          end do
          indx(ir) = indx(1)
          ir = ir - 1

          if ( ir .eq. 1 ) then
            indx(1) = indxt
            go to 30
          end if

        end if

        i = l
        j = l + l

20      continue

        if ( j .le. ir ) then

          if ( j .lt. ir ) then
            if (   a(1,indx(j)) .lt.  a(1,indx(j+1)) .or. 
     &           ( a(1,indx(j)) .eq. a(1,indx(j+1)) .and. 
     &             a(2,indx(j)) .lt.  a(2,indx(j+1)) ) ) then
              j = j + 1
            end if
          end if

          if (   aval(1) .lt.  a(1,indx(j)) .or. 
     &         ( aval(1) .eq. a(1,indx(j)) .and. 
     &           aval(2) .lt.  a(2,indx(j)) ) ) then
            indx(i) = indx(j)
            i = j
            j = j + j
          else
            j = ir + 1
          end if

          go to 20

        end if

        indx(i) = indxt

      go to 10

30    continue

      return
      end
      subroutine r82vec_sort_quick_a ( n, a )

c*********************************************************************72
c
cc R82VEC_SORT_QUICK_A ascending sorts an R82VEC using quick sort.
c
c  Discussion:
c
c    An R82VEC is an array of R82's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input/output, double precision A(2,N).
c    On input, the array to be sorted.
c    On output, the array has been sorted.
c
      implicit none

      integer level_max
      parameter ( level_max = 30 )
      integer n
      integer dim_num
      parameter ( dim_num = 2 )

      double precision a(dim_num,n)
      integer base
      integer l_segment
      integer level
      integer n_segment
      integer rsave(level_max)
      integer r_segment

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R82VEC_SORT_QUICK_A - Fatal error!'
        write ( *, '(a)' ) '  N < 1.'
        write ( *, '(a,i8)' ) '  N = ', n
        stop
      else if ( n .eq. 1 ) then
        return
      end if

      level = 1
      rsave(level) = n + 1
      base = 1
      n_segment = n

10    continue
c
c  Partition the segment.
c
        call r82vec_part_quick_a ( n_segment, a(1,base), l_segment, 
     &    r_segment )
c
c  If the left segment has more than one element, we need to partition it.
c
        if ( 1 .lt. l_segment ) then

          if ( level_max .lt. level ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 
     &        'R82VEC_SORT_QUICK_A - Fatal error!'
            write ( *, '(a,i8)' ) 
     &        '  Exceeding recursion maximum of ', level_max
            stop
          end if

          level = level + 1
          n_segment = l_segment
          rsave(level) = r_segment + base - 1
c
c  The left segment and the middle segment are sorted.
c  Must the right segment be partitioned?
c
        else if ( r_segment .lt. n_segment ) then

          n_segment = n_segment + 1 - r_segment
          base = base + r_segment - 1
c
c  Otherwise, we back up a level if there is an earlier one.
c
        else

20        continue

            if ( level .le. 1 ) then
              return
            end if

            base = rsave(level)
            n_segment = rsave(level-1) - rsave(level)
            level = level - 1

            if ( 0 .lt. n_segment ) then
              go to 30
            end if

          go to 20

30        continue

        end if

      go to 10

      return
      end
      subroutine r8mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, character*(*) TITLE, an optional title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character*(*) title

      call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, 
     &  jhi, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT transposed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8,6x)') i
        end do

        write ( *, '(''       Row'',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '       Col'

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc
            i = i2lo - 1 + i2
            write ( ctemp(i2), '(g14.6)' ) a(i,j)
          end do

          write ( *, '(2x,i8,5a14)' ) j, ( ctemp(i), i = 1, inc )

        end do

      end do

      return
      end
      subroutine r8mat_uniform_01 ( m, n, seed, r )

c*********************************************************************72
c
cc R8MAT_UNIFORM_01 returns a unit pseudorandom R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns in the array.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(M,N), the array of pseudorandom values.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      integer k
      integer seed
      double precision r(m,n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      do j = 1, n

        do i = 1, m

          k = seed / 127773

          seed = 16807 * ( seed - k * 127773 ) - k * 2836

          if ( seed .lt. 0 ) then
            seed = seed + 2147483647
          end if

          r(i,j) = dble ( seed ) * 4.656612875D-10

        end do
      end do

      return
      end
      subroutine r8tris2 ( node_num, node_xy, triangle_num, 
     &  triangle_node, triangle_neighbor )

c*********************************************************************72
c
cc R8TRIS2 constructs a Delaunay triangulation of 2D vertices.
c
c  Discussion:
c
c    The routine constructs the Delaunay triangulation of a set of 2D vertices
c    using an incremental approach and diagonal edge swaps.  Vertices are
c    first sorted in lexicographically increasing (X,Y) order, and
c    then are inserted one at a time from outside the convex hull.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 June 2009
c
c  Author:
c
c    Original FORTRAN77 version by Barry Joe.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Barry Joe,
c    GEOMPACK - a software package for the generation of meshes
c    using geometric algorithms,
c    Advances in Engineering Software,
c    Volume 13, pages 325-331, 1991.
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input/output, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates
c    of the nodes.  On output, the vertices have been sorted into 
c    dictionary order.
c
c    Output, integer TRIANGLE_NUM, the number of triangles in the 
c    triangulation;  TRIANGLE_NUM is equal to 2*NODE_NUM - NB - 2, where NB is 
c    the number of boundary vertices.
c
c    Output, integer TRIANGLE_NODE(3,TRIANGLE_NUM), the nodes that 
c    make up each triangle.  The elements are indices of P.  The vertices of
c    the triangles are in counter clockwise order.
c
c    Output, integer TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), the 
c    triangle neighbor list.  Positive elements are indices of TIL; negative 
c    elements are used for links of a counter clockwise linked list of boundary
c    edges;  LINK = -(3*I + J-1) where I, J = triangle, edge index;
c    TRIANGLE_NEIGHBOR(J,I) refers to the neighbor along edge from vertex J 
c    to J+1 (mod 3).
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer node_num

      double precision cmax
      integer dim
      integer e
      integer i
      integer ierr
      integer indx(node_num)
      integer j
      integer k
      integer l
      integer ledg
      integer lr
      integer lrline
      integer ltri
      integer m
      integer m1
      integer m2
      integer n
      double precision node_xy(dim_num,node_num)
      double precision r8_epsilon
      integer redg
      integer rtri
      integer stack(node_num)
      integer t
      double precision tol
      integer top
      integer triangle_neighbor(3,node_num*2)
      integer triangle_num
      integer triangle_node(3,node_num*2)

      tol = 100.0D+00 * r8_epsilon ( )

      ierr = 0
c
c  Sort the vertices by increasing (x,y).
c
      call r82vec_sort_heap_index_a ( node_num, node_xy, indx )

      call r82vec_permute ( node_num, indx, node_xy )
c
c  Make sure that the data nodes are "reasonably" distinct.
c
      m1 = 1

      do i = 2, node_num

        m = m1
        m1 = i

        k = 0

        do j = 1, dim_num

          cmax = max ( abs ( node_xy(j,m) ), abs ( node_xy(j,m1) ) )

          if ( tol * ( cmax + 1.0D+00 ) 
     &         .lt. abs ( node_xy(j,m) - node_xy(j,m1) ) ) then
            k = j
            go to 10
          end if

        end do

10      continue

        if ( k .eq. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
          write ( *, '(a,i8)' ) '  Fails for point number I = ', i
          write ( *, '(a,i8)' ) '  M = ', m
          write ( *, '(a,i8)' ) '  M1 = ', m1
          write ( *, '(a,2g14.6)' ) 
     &      '  NODE_XY(M)  = ', ( node_xy(dim,m), dim = 1, dim_num )
          write ( *, '(a,2g14.6)' ) 
     &      '  NODE_XY(M1) = ', ( node_xy(dim,m1), dim = 1, dim_num )
          ierr = 224
          stop
        end if

      end do
c
c  Starting from nodes M1 and M2, search for a third point M that
c  makes a "healthy" triangle (M1,M2,M)
c
      m1 = 1
      m2 = 2
      j = 3

20    continue

        if ( node_num .lt. j ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
          ierr = 225
          stop
        end if

        m = j

        lr = lrline ( node_xy(1,m), node_xy(2,m), node_xy(1,m1), 
     &    node_xy(2,m1), node_xy(1,m2), node_xy(2,m2), 0.0D+00 )

        if ( lr .ne. 0 ) then
          go to 30
        end if

        j = j + 1

      go to 20

30    continue
c
c  Set up the triangle information for (M1,M2,M), and for any other
c  triangles you created because points were collinear with M1, M2.
c
      triangle_num = j - 2

      if ( lr .eq. -1 ) then

        triangle_node(1,1) = m1
        triangle_node(2,1) = m2
        triangle_node(3,1) = m
        triangle_neighbor(3,1) = -3

        do i = 2, triangle_num

          m1 = m2
          m2 = i+1

          triangle_node(1,i) = m1
          triangle_node(2,i) = m2
          triangle_node(3,i) = m

          triangle_neighbor(1,i-1) = -3 * i
          triangle_neighbor(2,i-1) = i
          triangle_neighbor(3,i) = i - 1

        end do

        triangle_neighbor(1,triangle_num) = -3 * triangle_num - 1
        triangle_neighbor(2,triangle_num) = -5
        ledg = 2
        ltri = triangle_num

      else

        triangle_node(1,1) = m2
        triangle_node(2,1) = m1
        triangle_node(3,1) = m

        triangle_neighbor(1,1) = -4

        do i = 2, triangle_num

          m1 = m2
          m2 = i+1

          triangle_node(1,i) = m2
          triangle_node(2,i) = m1
          triangle_node(3,i) = m

          triangle_neighbor(3,i-1) = i
          triangle_neighbor(1,i) = -3 * i - 3
          triangle_neighbor(2,i) = i - 1

        end do

        triangle_neighbor(3,triangle_num) = -3 * triangle_num
        triangle_neighbor(2,1) = -3 * triangle_num - 2
        ledg = 2
        ltri = 1

      end if
c
c  Insert the vertices one at a time from outside the convex hull,
c  determine visible boundary edges, and apply diagonal edge swaps until
c  Delaunay triangulation of vertices (so far) is obtained.
c
      top = 0

      do i = j+1, node_num

        m = i
        m1 = triangle_node(ledg,ltri)

        if ( ledg <= 2 ) then
          m2 = triangle_node(ledg+1,ltri)
        else
          m2 = triangle_node(1,ltri)
        end if

        lr = lrline ( node_xy(1,m), node_xy(2,m), node_xy(1,m1), 
     &    node_xy(2,m1), node_xy(1,m2), node_xy(2,m2), 0.0D+00 )

        if ( 0 .lt. lr ) then
          rtri = ltri
          redg = ledg
          ltri = 0
        else
          l = -triangle_neighbor(ledg,ltri)
          rtri = l / 3
          redg = mod ( l, 3 ) + 1
        end if

        call vbedg ( node_xy(1,m), node_xy(2,m), node_num, node_xy, 
     &    triangle_num, triangle_node, triangle_neighbor, ltri, ledg, 
     &    rtri, redg )

        n = triangle_num + 1
        l = -triangle_neighbor(ledg,ltri)

40      continue

          t = l / 3
          e = mod ( l, 3 ) + 1
          l = -triangle_neighbor(e,t)
          m2 = triangle_node(e,t)

          if ( e <= 2 ) then
            m1 = triangle_node(e+1,t)
          else
            m1 = triangle_node(1,t)
          end if

          triangle_num = triangle_num + 1
          triangle_neighbor(e,t) = triangle_num

          triangle_node(1,triangle_num) = m1
          triangle_node(2,triangle_num) = m2
          triangle_node(3,triangle_num) = m

          triangle_neighbor(1,triangle_num) = t
          triangle_neighbor(2,triangle_num) = triangle_num - 1
          triangle_neighbor(3,triangle_num) = triangle_num + 1

          top = top + 1

          if ( node_num .lt. top ) then
            ierr = 8
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
            write ( *, '(a)' ) '  Stack overflow.'
            stop
          end if

          stack(top) = triangle_num

          if ( t .eq. rtri .and. e .eq. redg ) then
            go to 50
          end if

        go to 40

50      continue

        triangle_neighbor(ledg,ltri) = -3 * n - 1
        triangle_neighbor(2,n) = -3 * triangle_num - 2
        triangle_neighbor(3,triangle_num) = -l

        ltri = n
        ledg = 2

        call swapec ( m, top, ltri, ledg, node_num, node_xy, 
     &    triangle_num, triangle_node, triangle_neighbor, stack, 
     &    ierr )

        if ( ierr .ne. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
          write ( *, '(a)' ) '  Error return from SWAPEC.'
          stop
        end if

      end do
c
c  Now account for the sorting that we did.
c
      do i = 1, 3
        do j = 1, triangle_num
          triangle_node(i,j) = indx ( triangle_node(i,j) )
        end do
      end do

      call perm_inverse ( node_num, indx )

      call r82vec_permute ( node_num, indx, node_xy )

      return
      end
      function r8vec_eq ( n, a1, a2 )

c*********************************************************************72
c
cc R8VEC_EQ is true if every pair of entries in two vectors is equal.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vectors.
c
c    Input, double precision A1(N), A2(N), two vectors to compare.
c
c    Output, logical R8VEC_EQ.
c    R8VEC_EQ is .TRUE. if every pair of elements A1(I) and A2(I) are equal,
c    and .FALSE. otherwise.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i
      logical r8vec_eq

      r8vec_eq = .false.

      do i = 1, n
        if ( a1(i) .ne. a2(i) ) then
          return
        end if
      end do

      r8vec_eq = .true.

      return
      end
      function r8vec_gt ( n, a1, a2 )

c*********************************************************************72
c
cc R8VEC_GT .eq. ( A1 greater than A2 ) for double precision vectors.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The comparison is lexicographic.
c
c    A1 > A2  <=>                              A1(1) > A2(1) or
c                 ( A1(1)     .eq. A2(1)     and A1(2) > A2(2) ) or
c                 ...
c                 ( A1(1:N-1) .eq. A2(1:N-1) and A1(N) > A2(N)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision A1(N), A2(N), the vectors to be compared.
c
c    Output, logical R8VEC_GT, is TRUE if and only if A1 > A2.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i
      logical r8vec_gt

      r8vec_gt = .false.

      do i = 1, n

        if ( a2(i) .lt. a1(i) ) then
          r8vec_gt = .true.
          return
        else if ( a1(i) .lt. a2(i) ) then
          return
        end if

      end do

      return
      end
      function r8vec_lt ( n, a1, a2 )

c*********************************************************************72
c
cc R8VEC_LT .eq. ( A1 < A2 ) for double precision vectors.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The comparison is lexicographic.
c
c    A1 < A2  <=>                              A1(1) < A2(1) or
c                 ( A1(1)     .eq. A2(1)     and A1(2) < A2(2) ) or
c                 ...
c                 ( A1(1:N-1) .eq. A2(1:N-1) and A1(N) < A2(N)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision A1(N), A2(N), the vectors to be compared.
c
c    Output, logical R8VEC_LT, is TRUE if and only if A1 < A2.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i
      logical r8vec_lt

      r8vec_lt = .false.

      do i = 1, n

        if ( a1(i) .lt. a2(i) ) then
          r8vec_lt = .true.
          return
        else if ( a2(i) .lt. a1(i) ) then
          return
        end if

      end do

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do

      return
      end
      subroutine r8vec_swap ( n, a1, a2 )

c*********************************************************************72
c
cc R8VEC_SWAP swaps two R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the arrays.
c
c    Input/output, double precision A1(N), A2(N), the vectors to swap.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i
      double precision t

      do i = 1, n
        t     = a1(i)
        a1(i) = a2(i)
        a2(i) = t
      end do

      return
      end
      subroutine swapec ( i, top, btri, bedg, node_num, node_xy, 
     &  triangle_num, triangle_node, triangle_neighbor, stack, ierr )

c*********************************************************************72
c
cc SWAPEC swaps diagonal edges until all triangles are Delaunay.
c
c  Discussion:
c
c    The routine swaps diagonal edges in a 2D triangulation, based on
c    the empty circumcircle criterion, until all triangles are Delaunay,
c    given that I is the index of the new vertex added to the triangulation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2009
c
c  Author:
c
c    Original FORTRAN77 version by Barry Joe.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Barry Joe,
c    GEOMPACK - a software package for the generation of meshes
c    using geometric algorithms,
c    Advances in Engineering Software,
c    Volume 13, pages 325-331, 1991.
c
c  Parameters:
c
c    Input, integer I, the index of the new vertex.
c
c    Input/output, integer TOP, the index of the top of the stack.
c    On output, TOP is zero.
c
c    Input/output, integer BTRI, BEDG; on input, if positive, are 
c    the triangle and edge indices of a boundary edge whose updated indices
c    must be recorded.  On output, these may be updated because of swaps.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XY(2,NODE_NUM), the coordinates of the nodes.
c
c    Input, integer TRIANGLE_NUM, the number of triangles.
c
c    Input/output, integer TRIANGLE_NODE(3,TRIANGLE_NUM), the
c    triangle incidence list.  May be updated on output because of swaps.
c
c    Input/output, integer TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), the
c    triangle neighbor list; negative values are used for links of the 
c    counter-clockwise linked list of boundary edges;  May be updated on output 
c    because of swaps.
c      LINK = -(3*I + J-1) where I, J = triangle, edge index.
c
c    Workspace, integer STACK(MAXST); on input, entries 1 through TOP
c    contain the indices of initial triangles (involving vertex I)
c    put in stack; the edges opposite I should be in interior;  entries
c    TOP+1 through MAXST are used as a stack.
c
c    Output, integer IERR is set to 8 for abnormal return.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer node_num
      integer triangle_num

      integer a
      integer b
      integer bedg
      integer btri
      integer c
      integer diaedg
      integer e
      integer ee
      integer em1
      integer ep1
      integer f
      integer fm1
      integer fp1
      integer i
      integer ierr
      integer i4_wrap
      integer l
      double precision node_xy(dim_num,node_num)
      integer r
      integer s
      integer stack(node_num)
      integer swap
      integer t
      integer top
      integer triangle_node(3,triangle_num)
      integer triangle_neighbor(3,triangle_num)
      integer tt
      integer u
      double precision x
      double precision y
c
c  Determine whether triangles in stack are Delaunay, and swap
c  diagonal edge of convex quadrilateral if not.
c
      x = node_xy(1,i)
      y = node_xy(2,i)

10    continue

        if ( top .le. 0 ) then
          go to 40
        end if

        t = stack(top)
        top = top - 1

        if ( triangle_node(1,t) .eq. i ) then
          e = 2
          b = triangle_node(3,t)
        else if ( triangle_node(2,t) .eq. i ) then
          e = 3
          b = triangle_node(1,t)
        else
          e = 1
          b = triangle_node(2,t)
        end if

        a = triangle_node(e,t)
        u = triangle_neighbor(e,t)

        if ( triangle_neighbor(1,u) .eq. t ) then
          f = 1
          c = triangle_node(3,u)
        else if ( triangle_neighbor(2,u) .eq. t ) then
          f = 2
          c = triangle_node(1,u)
        else
          f = 3
          c = triangle_node(2,u)
        end if

        swap = diaedg ( x, y, node_xy(1,a), node_xy(2,a), node_xy(1,c), 
     &    node_xy(2,c), node_xy(1,b), node_xy(2,b) )

        if ( swap .eq. 1 ) then

          em1 = i4_wrap ( e - 1, 1, 3 )
          ep1 = i4_wrap ( e + 1, 1, 3 )
          fm1 = i4_wrap ( f - 1, 1, 3 )
          fp1 = i4_wrap ( f + 1, 1, 3 )

          triangle_node(ep1,t) = c
          triangle_node(fp1,u) = i

          r = triangle_neighbor(ep1,t)
          s = triangle_neighbor(fp1,u)

          triangle_neighbor(ep1,t) = u
          triangle_neighbor(fp1,u) = t
          triangle_neighbor(e,t) = s
          triangle_neighbor(f,u) = r

          if ( 0 .lt. triangle_neighbor(fm1,u) ) then
            top = top + 1
            stack(top) = u
          end if

          if ( 0 .lt. s ) then

            if ( triangle_neighbor(1,s) .eq. u ) then
              triangle_neighbor(1,s) = t
            else if ( triangle_neighbor(2,s) .eq. u ) then
              triangle_neighbor(2,s) = t
            else
              triangle_neighbor(3,s) = t
            end if

            top = top + 1

            if ( node_num .lt. top ) then
              ierr = 8
              return
            end if

            stack(top) = t

          else

            if ( u .eq. btri .and. fp1 .eq. bedg ) then
              btri = t
              bedg = e
            end if

            l = - ( 3 * t + e - 1 )
            tt = t
            ee = em1

20          continue

            if ( 0 < triangle_neighbor(ee,tt) ) then

              tt = triangle_neighbor(ee,tt)

              if ( triangle_node(1,tt) .eq. a ) then
                ee = 3
              else if ( triangle_node(2,tt) .eq. a ) then
                ee = 1
              else
                ee = 2
              end if

              go to 20

            end if

            triangle_neighbor(ee,tt) = l

          end if

          if ( 0 .lt. r ) then

            if ( triangle_neighbor(1,r) .eq. t ) then
              triangle_neighbor(1,r) = u
            else if ( triangle_neighbor(2,r) .eq. t ) then
              triangle_neighbor(2,r) = u
            else
              triangle_neighbor(3,r) = u
            end if

          else

            if ( t .eq. btri .and. ep1 .eq. bedg ) then
              btri = u
              bedg = f
            end if

            l = - ( 3 * u + f - 1 )
            tt = u
            ee = fm1

30          continue

            if ( 0 .lt. triangle_neighbor(ee,tt) ) then

              tt = triangle_neighbor(ee,tt)

              if ( triangle_node(1,tt) .eq. b ) then
                ee = 3
              else if ( triangle_node(2,tt) .eq. b ) then
                ee = 1
              else
                ee = 2
              end if

              go to 30

            end if

            triangle_neighbor(ee,tt) = l

          end if

        end if

      go to 10

40    continue

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ',
     &  'May      ', 'June     ', 'July     ', 'August   ',
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *,
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' )
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
      subroutine triangle_circumcenter_2d ( t, center )

c*********************************************************************72
c
cc TRIANGLE_CIRCUMCENTER_2D computes the circumcenter of a triangle in 2D.
c
c  Discussion:
c
c    The circumcenter of a triangle is the center of the circumcircle, the
c    circle that passes through the three vertices of the triangle.
c
c    The circumcircle contains the triangle, but it is not necessarily the
c    smallest triangle to do so.
c
c    If all angles of the triangle are no greater than 90 degrees, then
c    the center of the circumscribed circle will lie inside the triangle.
c    Otherwise, the center will lie outside the triangle.
c
c    The circumcenter is the intersection of the perpendicular bisectors
c    of the sides of the triangle.
c
c    In geometry, the circumcenter of a triangle is often symbolized by "O".
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the triangle vertices.
c
c    Output, double precision CENTER(2), the circumcenter of the triangle.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )

      double precision asq
      double precision bot
      double precision center(dim_num)
      double precision csq
      double precision t(dim_num,3)
      double precision top(dim_num)

      asq = ( t(1,2) - t(1,1) )**2 + ( t(2,2) - t(2,1) )**2
      csq = ( t(1,3) - t(1,1) )**2 + ( t(2,3) - t(2,1) )**2
      
      top(1) =    ( t(2,2) - t(2,1) ) * csq - ( t(2,3) - t(2,1) ) * asq
      top(2) =  - ( t(1,2) - t(1,1) ) * csq + ( t(1,3) - t(1,1) ) * asq

      bot  =  ( t(2,2) - t(2,1) ) * ( t(1,3) - t(1,1) ) 
     &      - ( t(2,3) - t(2,1) ) * ( t(1,2) - t(1,1) )

      center(1:2) = t(1:2,1) + 0.5D+00 * top(1:2) / bot

      return
      end
      subroutine triangulation_order3_plot ( file_name, node_num, 
     &  node_xy, triangle_num, triangle_node, node_show, 
     &  triangle_show )

c*********************************************************************72
c
cc TRIANGULATION_ORDER3_PLOT plots a 3-node triangulation of a set of nodes.
c
c  Discussion:
c
c    The triangulation is most usually a Delaunay triangulation,
c    but this is not necessary.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILE_NAME, the name of the output file.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XY(2,NODE_NUM), the coordinates of the nodes.
c
c    Input, integer TRIANGLE_NUM, the number of triangles.
c
c    Input, integer TRIANGLE_NODE(3,TRIANGLE_NUM), lists, for 
c    each triangle, the indices of the nodes that form the vertices of the 
c    triangle.
c
c    Input, integer NODE_SHOW,
c    0, do not show nodes;
c    1, show nodes;
c    2, show nodes and label them.
c
c    Input, integer TRIANGLE_SHOW,
c    0, do not show triangles;
c    1, show triangles;
c    2, show triangles and label them.
c
      implicit none

      integer node_num
      integer triangle_num
      integer triangle_order
      parameter ( triangle_order = 3 )

      double precision ave_x
      double precision ave_y
      integer circle_size
      integer delta
      integer e
      character * ( * )  file_name
      integer file_unit
      integer i
      integer i4_wrap
      integer ios
      integer node
      integer node_show
      double precision node_xy(2,node_num)
      character * ( 40 ) string
      integer triangle
      integer triangle_node(triangle_order,triangle_num)
      integer triangle_show
      double precision x_max
      double precision x_min
      integer x_ps
      integer x_ps_max
      integer x_ps_max_clip
      integer x_ps_min
      integer x_ps_min_clip
      double precision x_scale
      double precision y_max
      double precision y_min
      integer y_ps
      integer y_ps_max
      integer y_ps_max_clip
      integer y_ps_min
      integer y_ps_min_clip
      double precision y_scale
c
c  We need to do some figuring here, so that we can determine
c  the range of the data, and hence the height and width
c  of the piece of paper.
c
      x_max = node_xy(1,1)
      x_min = node_xy(1,1)
      do node = 2, node_num
        x_max = max ( x_max, node_xy(1,node) )
        x_min = min ( x_min, node_xy(1,node) )
      end do
      x_scale = x_max - x_min

      x_max = x_max + 0.05D+00 * x_scale
      x_min = x_min - 0.05D+00 * x_scale
      x_scale = x_max - x_min

      y_max = node_xy(2,1)
      y_min = node_xy(2,1)
      do node = 2, node_num
        y_max = max ( y_max, node_xy(2,node) )
        y_min = min ( y_min, node_xy(2,node) )
      end do
      y_scale = y_max - y_min

      y_max = y_max + 0.05D+00 * y_scale
      y_min = y_min - 0.05D+00 * y_scale
      y_scale = y_max - y_min

      x_ps_max = 576
      x_ps_max_clip = 594
      x_ps_min = 36
      x_ps_min_clip = 18
      y_ps_max = 666
      y_ps_max_clip = 684
      y_ps_min = 126
      y_ps_min_clip = 108
      
      if ( x_scale .lt. y_scale ) then

        delta = nint ( dble ( x_ps_max - x_ps_min ) 
     &    * ( y_scale - x_scale ) / ( 2.0D+00 * y_scale ) )

        x_ps_max = x_ps_max - delta
        x_ps_min = x_ps_min + delta

        x_ps_max_clip = x_ps_max_clip - delta
        x_ps_min_clip = x_ps_min_clip + delta

        x_scale = y_scale

      else if ( y_scale .lt. x_scale ) then

        delta = nint ( dble ( y_ps_max - y_ps_min ) 
     &    * ( x_scale - y_scale ) / ( 2.0D+00 * x_scale ) )

        y_ps_max      = y_ps_max - delta
        y_ps_min      = y_ps_min + delta

        y_ps_max_clip = y_ps_max_clip - delta
        y_ps_min_clip = y_ps_min_clip + delta

        y_scale = x_scale

      end if

      call get_unit ( file_unit )

      open ( unit = file_unit, file = file_name, status = 'replace' )

      write ( file_unit, '(a)' ) '%!PS-Adobe-3.0 EPSF-3.0'
      write ( file_unit, '(a)' ) 
     &  '%%Creator: triangulation_order3_plot.f'
      write ( file_unit, '(a)' ) '%%Title: ' // trim ( file_name )
      write ( file_unit, '(a)' ) '%%Pages: 1'
      write ( file_unit, '(a,i3,2x,i3,2x,i3,2x,i3)' ) '%%BoundingBox: ',
     &  x_ps_min, y_ps_min, x_ps_max, y_ps_max
      write ( file_unit, '(a)' ) '%%Document-Fonts: Times-Roman'
      write ( file_unit, '(a)' ) '%%LanguageLevel: 1'
      write ( file_unit, '(a)' ) '%%EndComments'
      write ( file_unit, '(a)' ) '%%BeginProlog'
      write ( file_unit, '(a)' ) '/inch {72 mul} def'
      write ( file_unit, '(a)' ) '%%EndProlog'
      write ( file_unit, '(a)' ) '%%Page: 1 1'
      write ( file_unit, '(a)' ) 'save'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) 
     &  '%  Increase line width from default 0.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '2 setlinewidth'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) 
     &  '%  Set the RGB line color to very light gray.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '0.900  0.900  0.900 setrgbcolor'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) 
     &  '%  Draw a gray border around the page.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) 'newpath'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) 
     &  '  ', x_ps_min, y_ps_min, ' moveto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) 
     &  '  ', x_ps_max, y_ps_min, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) 
     &  '  ', x_ps_max, y_ps_max, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) 
     &  '  ', x_ps_min, y_ps_max, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) 
     &  '  ', x_ps_min, y_ps_min, ' lineto'
      write ( file_unit, '(a)' ) 'stroke'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Set the RGB color to black.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '0.000  0.000  0.000 setrgbcolor'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Set the font and its size.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '/Times-Roman findfont'
      write ( file_unit, '(a)' ) '0.50 inch scalefont'
      write ( file_unit, '(a)' ) 'setfont'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Print a title.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  210  702  moveto'
      write ( file_unit, '(a)' ) '%  (Triangulation)  show'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Define a clipping polygon.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) 'newpath'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', 
     &  x_ps_min_clip, y_ps_min_clip, ' moveto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', 
     &  x_ps_max_clip, y_ps_min_clip, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', 
     &  x_ps_max_clip, y_ps_max_clip, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', 
     &  x_ps_min_clip, y_ps_max_clip, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', 
     &  x_ps_min_clip, y_ps_min_clip, ' lineto'
      write ( file_unit, '(a)' ) 'clip newpath'
c
c  Draw the nodes.
c
      if ( node_num .le. 200 ) then
        circle_size = 5
      else if ( node_num .le. 500 ) then
        circle_size = 4
      else if ( node_num .le. 1000 ) then
        circle_size = 3
      else if ( node_num .le. 5000 ) then
        circle_size = 2
      else
        circle_size = 1
      end if

      if ( 1 .le. node_show ) then
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '%  Draw filled dots at the nodes.'
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '%  Set the RGB color to blue.'
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '0.000  0.150  0.750 setrgbcolor'
        write ( file_unit, '(a)' ) '%'

        do node = 1, node_num

          x_ps = int ( 
     &      ( ( x_max - node_xy(1,node)         ) * dble ( x_ps_min )   
     &      + (         node_xy(1,node) - x_min ) * dble ( x_ps_max ) ) 
     &      / ( x_max                   - x_min ) )

          y_ps = int ( 
     &      ( ( y_max - node_xy(2,node)         ) * dble ( y_ps_min )   
     &      + (         node_xy(2,node) - y_min ) * dble ( y_ps_max ) ) 
     &      / ( y_max                   - y_min ) )

          write ( file_unit, '(a,i4,2x,i4,2x,i4,2x,a)' ) 
     &      'newpath ', x_ps, y_ps, 
     &      circle_size, '0 360 arc closepath fill'

        end do

      end if
c
c  Label the nodes.
c
      if ( 2 .le. node_show ) then

        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '%  Label the nodes:'
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) 
     &    '%  Set the RGB color to darker blue.'
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '0.000  0.250  0.850 setrgbcolor'
        write ( file_unit, '(a)' ) '/Times-Roman findfont'
        write ( file_unit, '(a)' ) '0.20 inch scalefont'
        write ( file_unit, '(a)' ) 'setfont'
        write ( file_unit, '(a)' ) '%'

        do node = 1, node_num

          x_ps = int ( 
     &      ( ( x_max - node_xy(1,node)         ) * dble ( x_ps_min )   
     &      + (       + node_xy(1,node) - x_min ) * dble ( x_ps_max ) ) 
     &      / ( x_max                   - x_min ) )

          y_ps = int ( 
     &      ( ( y_max - node_xy(2,node)         ) * dble ( y_ps_min )   
     &      + (         node_xy(2,node) - y_min ) * dble ( y_ps_max ) ) 
     &      / ( y_max                   - y_min ) )

          write ( string, '(i4)' ) node
          string = adjustl ( string )

          write ( file_unit, '(i4,2x,i4,a)' ) x_ps, y_ps+5, 
     &      ' moveto (' // trim ( string ) // ') show'

        end do

      end if
c
c  Draw the triangles.
c
      if ( 1 .le. triangle_show ) then
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '%  Set the RGB color to red.'
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '0.900  0.200  0.100 setrgbcolor'
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '%  Draw the triangles.'
        write ( file_unit, '(a)' ) '%'

        do triangle = 1, triangle_num

          write ( file_unit, '(a)' ) 'newpath'

          do i = 1, 4

            e = i4_wrap ( i, 1, 3 )

            node = triangle_node(e,triangle)

            x_ps = int ( 
     &        ( ( x_max - node_xy(1,node)         ) 
     &        * dble ( x_ps_min )   
     &        + (         node_xy(1,node) - x_min ) 
     &        * dble ( x_ps_max ) )
     &        / ( x_max                   - x_min ) )

            y_ps = int ( 
     &        ( ( y_max - node_xy(2,node)         ) 
     &        * dble ( y_ps_min )   
     &        + (         node_xy(2,node) - y_min ) 
     &        * dble ( y_ps_max ) )
     &        / ( y_max                   - y_min ) )

            if ( i .eq. 1 ) then
              write ( file_unit, '(i3,2x,i3,2x,a)' ) 
     &          x_ps, y_ps, ' moveto'
            else
              write ( file_unit, '(i3,2x,i3,2x,a)' ) 
     &          x_ps, y_ps, ' lineto'
            end if

          end do

          write ( file_unit, '(a)' ) 'stroke'

        end do

      end if
c
c  Label the triangles.
c
      if ( 2 .le. triangle_show ) then

        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '%  Label the triangles:'
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '%  Set the RGB color to darker red.'
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '0.950  0.250  0.150 setrgbcolor'
        write ( file_unit, '(a)' ) '/Times-Roman findfont'
        write ( file_unit, '(a)' ) '0.20 inch scalefont'
        write ( file_unit, '(a)' ) 'setfont'
        write ( file_unit, '(a)' ) '%'

        do triangle = 1, triangle_num

          ave_x = 0.0D+00
          ave_y = 0.0D+00

          do i = 1, 3

            node = triangle_node(i,triangle)

            ave_x = ave_x + node_xy(1,node)
            ave_y = ave_y + node_xy(2,node)

          end do

          ave_x = ave_x / 3.0D+00
          ave_y = ave_y / 3.0D+00

          x_ps = int ( 
     &      ( ( x_max - ave_x         ) * dble ( x_ps_min )   
     &      + (       + ave_x - x_min ) * dble ( x_ps_max ) ) 
     &      / ( x_max         - x_min ) )

          y_ps = int ( 
     &      ( ( y_max - ave_y         ) * dble ( y_ps_min )   
     &      + (         ave_y - y_min ) * dble ( y_ps_max ) ) 
     &      / ( y_max         - y_min ) )

          write ( string, '(i4)' ) triangle
          string = adjustl ( string )

          write ( file_unit, '(i4,2x,i4,a)' ) x_ps, y_ps, ' moveto (' 
     &      // trim ( string ) // ') show'

        end do

      end if

      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) 'restore  showpage'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  End of page.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%%Trailer'
      write ( file_unit, '(a)' ) '%%EOF'
      close ( unit = file_unit )

      return
      end
      subroutine triangulation_order3_print ( node_num, triangle_num, 
     &  node_xy, triangle_node, triangle_neighbor )

c*********************************************************************72
c
cc TRIANGULATION_ORDER3_PRINT prints information about a triangulation.
c
c  Discussion:
c
c    Triangulations created by R8TRIS2 include extra information encoded
c    in the negative values of TRIANGLE_NEIGHBOR.
c
c    Because some of the nodes counted in NODE_NUM may not actually be
c    used in the triangulation, I needed to compute the true number
c    of vertices.  I added this calculation on 13 October 2001.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer TRIANGLE_NUM, the number of triangles.
c
c    Input, double precision NODE_XY(2,NODE_NUM), the coordinates of the nodes.
c
c    Input, integer TRIANGLE_NODE(3,TRIANGLE_NUM), the nodes 
c    that make up the triangles.
c
c    Input, integer TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), the 
c    triangle neighbors on each side.  If there is no triangle neighbor on 
c    a particular side, the value of TRIANGLE_NEIGHBOR should be negative.  
c    If the triangulation data was created by R8TRIS22, then there is more 
c    information encoded in the negative values.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer node_num
      integer triangle_num
      integer triangle_order
      parameter ( triangle_order = 3 )

      integer boundary_num
      integer i
      integer i4_wrap
      integer j
      integer k
      integer n1
      integer n2
      double precision node_xy(dim_num,node_num)
      integer s
      logical skip
      integer t
      integer triangle_node(triangle_order,triangle_num)
      integer triangle_neighbor(3,triangle_num)
      integer vertex_list(triangle_order*triangle_num)
      integer vertex_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGULATION_ORDER3_PRINT'
      write ( *, '(a)' ) 
     &  '  Information defining an order3 triangulation.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The number of nodes is ', node_num

      call r8mat_transpose_print ( dim_num, node_num, node_xy, 
     &  '  Node coordinates' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  The number of triangles is ', triangle_num
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Sets of three nodes are used as vertices of'
      write ( *, '(a)' ) 
     &  '  the triangles.  For each triangle, the nodes'
      write ( *, '(a)' ) '  are listed in counterclockwise order.'

      call i4mat_transpose_print ( 3, triangle_num, triangle_node, 
     &  '  Triangle nodes:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  On each side of a given triangle, there is either'
      write ( *, '(a)' ) 
     &  '  another triangle, or a piece of the convex hull.'
      write ( *, '(a)' ) 
     &  '  For each triangle, we list the indices of the three'
      write ( *, '(a)' ) 
     &  '  neighbors, or (if negative) the codes of the'
      write ( *, '(a)' ) '  segments of the convex hull.'

      call i4mat_transpose_print ( 3, triangle_num, triangle_neighbor, 
     &  '  Triangle neighbors' )
c
c  Determine the number of vertices.  
c
      k = 0
      do j = 1, triangle_num
        do i = 1, 3
          k = k + 1
          vertex_list(k) = triangle_node(i,j)
        end do
      end do

      call i4vec_sort_heap_a ( 3*triangle_num, vertex_list )

      call i4vec_sorted_unique ( 3*triangle_num, vertex_list, 
     &  vertex_num )
c
c  Determine the number of boundary points.
c
      boundary_num = 2 * vertex_num - triangle_num - 2

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  The number of boundary points is ', boundary_num
       
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  The segments that make up the convex hull can be'
      write ( *, '(a)' ) 
     &  '  determined from the negative entries of the triangle'
      write ( *, '(a)' ) '  neighbor list.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     #   Tri  Side    N1    N2'
      write ( *, '(a)' ) ' '

      skip = .false.

      k = 0

      do i = 1, triangle_num

        do j = 1, 3

          if ( triangle_neighbor(j,i) .lt. 0 ) then
            s = - triangle_neighbor(j,i)
            t = s / 3

            if ( t .lt. 1 .or. triangle_num .lt. t ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 
     &          '  Sorry, this data does not use the R8TRIS2'
              write ( *, '(a)' ) 
     &          '  convention for convex hull segments.'
              skip = .true.
              exit
            end if

            s = mod ( s, 3 ) + 1
            k = k + 1
            n1 = triangle_node(s,t)
            n2 = triangle_node(i4_wrap(s+1,1,3),t)
            write ( *, '(2x,i4,2x,i4,2x,i4,2x,i4,2x,i4)' ) 
     &        k, t, s, n1, n2
          end if

        end do

        if ( skip ) then
          exit
        end if

      end do

      return
      end
      subroutine vbedg ( x, y, node_num, node_xy, triangle_num, 
     &  triangle_node, triangle_neighbor, ltri, ledg, rtri, redg )

c*********************************************************************72
c
cc VBEDG determines which boundary edges are visible to a point.
c
c  Discussion:
c
c    The point (X,Y) is assumed to be outside the convex hull of the
c    region covered by the 2D triangulation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 June 2009
c
c  Author:
c
c    Original FORTRAN77 version by Barry Joe.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Barry Joe,
c    GEOMPACK - a software package for the generation of meshes
c    using geometric algorithms,
c    Advances in Engineering Software,
c    Volume 13, pages 325-331, 1991.
c
c  Parameters:
c
c    Input, double precision X, Y, the coordinates of a point outside the
c    convex hull of the current triangulation.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XY(2,NODE_NUM), the coordinates of the nodes.
c
c    Input, integer TRIANGLE_NUM, the number of triangles.
c
c    Input, integer TRIANGLE_NODE(3,TRIANGLE_NUM), the triangle 
c    incidence list.
c
c    Input, integer TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), the 
c    triangle neighbor list; negative values are used for links of a 
c    counter clockwise linked list of boundary edges;
c      LINK = -(3*I + J-1) where I, J = triangle, edge index.
c
c    Input/output, integer LTRI, LEDG.  If LTRI /= 0 then these 
c    values are assumed to be already computed and are not changed, else they 
c    are updated.  On output, LTRI is the index of boundary triangle to the 
c    left of the leftmost boundary triangle visible from (X,Y), and LEDG is 
c    the boundary edge of triangle LTRI to the left of the leftmost boundary 
c    edge visible from (X,Y).  1 <= LEDG <= 3.
c
c    Input/output, integer RTRI.  On input, the index of the 
c    boundary triangle to begin the search at.  On output, the index of the 
c    rightmost boundary triangle visible from (X,Y).
c
c    Input/output, integer REDG, the edge of triangle RTRI that 
c    is visible from (X,Y).  1 <= REDG <= 3.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer node_num
      integer triangle_num

      integer a
      integer b
      integer e
      integer i4_wrap
      integer l
      logical ldone
      integer ledg
      integer lr
      integer lrline
      integer ltri
      double precision node_xy(2,node_num)
      integer redg
      integer rtri
      integer t
      integer triangle_node(3,triangle_num)
      integer triangle_neighbor(3,triangle_num)
      double precision x
      double precision y
c
c  Find the rightmost visible boundary edge using links, then possibly
c  leftmost visible boundary edge using triangle neighbor information.
c
      if ( ltri .eq. 0 ) then
        ldone = .false.
        ltri = rtri
        ledg = redg
      else
        ldone = .true.
      end if

10    continue

        l = - triangle_neighbor(redg,rtri)
        t = l / 3
        e = mod ( l, 3 ) + 1
        a = triangle_node(e,t)

        if ( e .le. 2 ) then
          b = triangle_node(e+1,t)
        else
          b = triangle_node(1,t)
        end if

        lr = lrline ( x, y, node_xy(1,a), node_xy(2,a), node_xy(1,b), 
     &    node_xy(2,b), 0.0D+00 )

        if ( lr .le. 0 ) then
          go to 20
        end if

        rtri = t
        redg = e

      go to 10

20    continue

      if ( ldone ) then
        return
      end if

      t = ltri
      e = ledg

30    continue

        b = triangle_node(e,t)
        e = e - 1
        e = i4_wrap ( e, 1, 3 )

40      continue

        if ( 0 .lt. triangle_neighbor(e,t) ) then

          t = triangle_neighbor(e,t)

          if ( triangle_node(1,t) .eq. b ) then
            e = 3
          else if ( triangle_node(2,t) .eq. b ) then
            e = 1
          else
            e = 2
          end if

          go to 40

        end if

        a = triangle_node(e,t)

        lr = lrline ( x, y, node_xy(1,a), node_xy(2,a), node_xy(1,b), 
     &    node_xy(2,b), 0.0D+00 )

        if ( lr .le. 0 ) then
          go to 50
        end if

      go to 30

50    continue

      ltri = t
      ledg = e

      return
      end