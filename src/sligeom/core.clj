(ns sligeom.core
  (:use slimath.core))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord Intersection [point normal])

(defprotocol Transformable
  (transform [this T] "Transform the object by T"))
    
(defn point3 "Construct a point3" [^double x ^double y ^double z]
  [x y z 1.0])

(defn vector3 "Construct a vector3" [^double x ^double y ^double z]
  [x y z 0.0])

(defn normal "Construct a normal vector" [^double x ^double y ^double z]
  [x y z 0.0])
