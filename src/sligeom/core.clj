(ns sligeom.core
  (:use slimath.core)
  (:require [clojure.math.numeric-tower :as numeric]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defprotocol RayIntersection
  ( intersect [this r] "Distance to intersection"))

(defprotocol Transformable
  (transform [this T] "Transform the object by T"))

(defprotocol Bounded
  (bounding-box [this] "Compute the AABB"))

(deftype Intersection [point normal])
    
(definterface Bounding
  (^double width [])
  (^double height [])
  (^double depth []))

(defn point3 "Construct a point3" [^double x ^double y ^double z]
  [x y z 1.0])

(defn vector3 "Construct a vector3" [^double x ^double y ^double z]
  [x y z 0.0])

(defn normal "Construct a normal vector" [^double x ^double y ^double z]
  [x y z 0.0])

(load "transform")
(load "ray")
(load "bbox")
(load "sphere")
(load "triangle")
