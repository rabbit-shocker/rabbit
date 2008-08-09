# Based on trackball.rb in the Logo demo code:
#   http://ruby-gnome2.sourceforge.jp/hiki.cgi?Logo
#
# Original code license is:
#
=begin
  trackball.rb
    Implementation of a virtual trackball and vector.

    Copyright (c) 2004 Masao Mutoh

    You can redistribute it and/or modify it under the terms of
    the original code license below.

    Original code license is:

/*
 * (c) Copyright 1993, 1994, Silicon Graphics, Inc.
 * ALL RIGHTS RESERVED
 * Permission to use, copy, modify, and distribute this software for
 * any purpose and without fee is hereby granted, provided that the above
 * copyright notice appear in all copies and that both the copyright notice
 * and this permission notice appear in supporting documentation, and that
 * the name of Silicon Graphics, Inc. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.
 *
 * THE MATERIAL EMBODIED ON THIS SOFTWARE IS PROVIDED TO YOU "AS-IS"
 * AND WITHOUT WARRANTY OF ANY KIND, EXPRESS, IMPLIED OR OTHERWISE,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY OR
 * FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL SILICON
 * GRAPHICS, INC.  BE LIABLE TO YOU OR ANYONE ELSE FOR ANY DIRECT,
 * SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY
 * KIND, OR ANY DAMAGES WHATSOEVER, INCLUDING WITHOUT LIMITATION,
 * LOSS OF PROFIT, LOSS OF USE, SAVINGS OR REVENUE, OR THE CLAIMS OF
 * THIRD PARTIES, WHETHER OR NOT SILICON GRAPHICS, INC.  HAS BEEN
 * ADVISED OF THE POSSIBILITY OF SUCH LOSS, HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE
 * POSSESSION, USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * US Government Users Restricted Rights
 * Use, duplication, or disclosure by the Government is subject to
 * restrictions set forth in FAR 52.227.19(c)(2) or subparagraph
 * (c)(1)(ii) of the Rights in Technical Data and Computer Software
 * clause at DFARS 252.227-7013 and/or in similar or successor
 * clauses in the FAR or the DOD or NASA FAR Supplement.
 * Unpublished-- rights reserved under the copyright laws of the
 * United States.  Contractor/manufacturer is Silicon Graphics,
 * Inc., 2011 N.  Shoreline Blvd., Mountain View, CA 94039-7311.
 *
 * OpenGL(TM) is a trademark of Silicon Graphics, Inc.
 */
/*
 * Trackball code:
 *
 * Implementation of a virtual trackball.
 * Implemented by Gavin Bell, lots of ideas from Thant Tessman and
 *   the August '88 issue of Siggraph's "Computer Graphics," pp. 121-129.
 *
 * Vector manip code:
 *
 * Original code from:
 * David M. Ciemiewicz, Mark Grossman, Henry Moreton, and Paul Haeberli
 *
 * Much mucking with by:
 * Gavin Bell
 */
=end

module Rabbit
  module TrackBall

    class Vector < Array
      def vsub(other, range = nil)
        range = [size, other.size].min unless range
        (0...range).inject(Vector.new){|ret, i| ret << self[i] - other[i]}
      end
      
      def vcross(other)
        dst = Vector.new
        dst << (self[1] * other[2]) - (self[2] * other[1])
        dst << (self[2] * other[0]) - (self[0] * other[2])
        dst << (self[0] * other[1]) - (self[1] * other[0])
      end
      
      def vlength
        Math.sqrt(inject(0){|ret, i| ret + i ** 2})
      end
      
      def vscale(div); collect{|v| v *= div}; end
      def vnormal; vscale(1.0 / vlength); end
      
      def vdot(other, range = nil)
        range = [size, other.size].min unless range
        (0...range).inject(0.0){|ret, i| ret + self[i] * other[i]}
      end
      
      def vadd(other, range = nil)
        range = [size, other.size].min unless range
        (0...range).inject(Vector.new){|ret, i| ret << self[i] + other[i]}
      end
      
      #
      # Quaternions always obey:  a^2 + b^2 + c^2 + d^2 = 1.0
      # If they don't add up to 1.0, dividing by their magnitued will
      # renormalize them.
      #
      # Note: See the following for more information on quaternions:
      #
      # - Shoemake, K., Animating rotation with quaternion curves, Computer
      #   Graphics 19, No 3 (Proc. SIGGRAPH'85), 245-254, 1985.
      # - Pletinckx, D., Quaternion calculus as a basic tool in computer
      #   graphics, The Visual Computer 5, 2-13, 1989.
      #
      def normalize_quat
        collect{|q| q = q / inject(0){|ret, i| ret + i ** 2}}
      end
      
      # Build a rotation matrix, given a quaternion rotation.
      def build_rotmatrix
        m = []
        m << [1.0 - 2.0 * (self[1] * self[1] + self[2] * self[2]),
              2.0 * (self[0] * self[1] - self[2] * self[3]),
              2.0 * (self[2] * self[0] + self[1] * self[3]), 0.0]
        m << [2.0 * (self[0] * self[1] + self[2] * self[3]),
              1.0 - 2.0 * (self[2] * self[2] + self[0] * self[0]),
              2.0 * (self[1] * self[2] - self[0] * self[3]), 0.0]
        m << [2.0 * (self[2] * self[0] - self[1] * self[3]),
              2.0 * (self[1] * self[2] + self[0] * self[3]),
              1.0 - 2.0 * (self[1] * self[1] + self[0] * self[0]), 0.0]
        m << [0.0, 0.0, 0.0, 1.0]
        m
      end
      
      # Given an axis and angle, compute quaternion.
      def axis_to_quat(phi)
        b, c, d = self.vnormal.vscale(Math.sin(phi/2.0))
        Vector.new([b, c, d, Math.cos(phi/2.0)])
      end

      def collect(&block)
        Vector.new(super)
      end
    end

    # 
    # This size should really be based on the distance from the center of
    # rotation to the point on the object underneath the mouse.  That
    # point would then track the mouse as closely as possible.  This is a
    # simple example, though, so that is left as an Exercise for the
    # Programmer.
    #
    SIZE = 0.8

    module_function
    def trackball(p1x, p1y, p2x, p2y)
      if (p1x == p2x && p1y == p2y)
        return Vector.new([0.0, 0.0, 0.0, 1.0])
      end
    
      # First, figure out z-coordinates for projection of P1 and P2 to
      # deformed sphere
      p1 = Vector.new([p1x, p1y, tb_project_to_sphere(SIZE, p1x, p1y)])
      p2 = Vector.new([p2x, p2y, tb_project_to_sphere(SIZE, p2x, p2y)])

      # Now, we want the cross product of P1 and P2
      a = p2.vcross(p1)
      
      # Figure out how much to rotate around that axis.
      d = p1.vsub(p2)
      t = d.vlength / (2.0 * SIZE)
      
      # Avoid problems with out-of-control values...
      t = 1.0 if (t > 1.0)
      t = -1.0 if (t < -1.0)
      phi = 2.0 * Math.asin(t)
      
      a.axis_to_quat(phi)
    end
    
    #
    # Project an x,y pair onto a sphere of radius r OR a hyperbolic sheet
    # if we are away from the center of the sphere.
    #
    def tb_project_to_sphere(r, x, y)
      d = Math.sqrt(x * x + y * y)
      if (d < r * 0.70710678118654752440)     # Inside sphere
        z = Math.sqrt(r * r - d * d)
      else          # On hyperbola
        t = r / 1.41421356237309504880
        z = t * t / d
      end
      z
    end
    
    #
    # Given two rotations, e2 and e2, expressed as quaternion rotations,
    # figure out the equivalent single rotation and stuff it into dest.
    # 
    # This routine also normalizes the result every RENORM_COUNT times it is
    # called, to keep error from creeping in.
    # 
    # NOTE: This routine is written so that q1 or q2 may be the same
    # as dest (or each other).
    #
    
    RENORM_COUNT = 97
    
    @@quats_count = 0
    def add_quats(q1, q2)
      t1 = q1.vscale(q2[3])
      t2 = q2.vscale(q1[3])
      t3 = q2.vcross(q1)
      tf = t1.vadd(t2)
      tf = t3.vadd(tf)
      
      tf[3] = q1[3] * q2[3] - q1.vdot(q2, 3)
      
      @@quats_count += 1
      ret = tf
      if (@@quats_count > RENORM_COUNT)
        @@quats_count = 0
        ret = tf.normalize_quat
      end
      ret
    end
  end
end
