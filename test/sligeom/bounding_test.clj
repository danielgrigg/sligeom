(ns sligeom.bounding-test
  (:use midje.sweet
        sligeom.core
        sligeom.bounding)
  (:require [clojure.string :as str]))

(fact "`bbox` creates an AABB from two points"
      (bbox (point3 -1.1 2.2 -3.3)
            (point3 -5.5 9.9 7.7)) => {:minp (point3 -5.5 2.2 -3.3)
                                       :maxp (point3 -1.1 9.9 7.7)})

(fact "`bbox-centre` returns the centre point"
      (bbox-centre (bbox (point3 2 3 5) (point3 7 13 9)))
      => (point3 4.5 8.0 7.0))

(fact "`bbox-size` returns the bbox size"
      (bbox-size (bbox (point3 2 3 5) (point3 7 13 9)))
      => (vector3 5 10 4))

(fact "`bbox-union` bbox unioned with point3"
      (bbox-union (bbox (point3 2 3 5) (point3 7 13 9))
                  (point3 35 -21 23))
      => (bbox (point3 2 -21 5) (point3 35 13 23)))

(fact "`contains-point?` bbox contains a point p"
      (contains-point? (bbox (point3 2 3 5) (point3 7 13 9)) (point3 5 8 9))
      => true
      (contains-point? (bbox (point3 2 3 5) (point3 7 13 9)) (point3 7.5 8 8))
      => false
      (contains-point? (bbox (point3 2 3 5) (point3 7 13 9)) (point3 2 13.4 8))
      => false
      (contains-point? (bbox (point3 2 3 5) (point3 7 13 9)) (point3 2 13 9.2))
      => false
)