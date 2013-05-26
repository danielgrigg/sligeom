(ns sligeom.intersect-test
  (:use midje.sweet
        sligeom.core
        sligeom.intersect)
  (:require [clojure.string :as str]))

(fact "`ray` creates a ray"
      (ray (point3 -1.1 3.3 2.2) (vector3 3.3 5.5 4.4)) 
      => {:origin [-1.1 3.3 2.2 1.0], :direction [3.3 5.5 4.4 0.0], :mint 4.0E-5, :maxt 1.0E37})

(fact "`ray-at` computes ray at t"
      (ray-at (ray (point3 2 5 3) (vector3 -1 2 4)) 1.0)
      => (point3 1.0 7.0 7.0)
      (ray-at (ray (point3 2 5 3) (vector3 -1 2 4)) 0.4)
      => (point3 1.6 5.8 4.6))