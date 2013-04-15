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

(load "transform")
(load "ray")
(load "bbox")
(load "sphere")
(load "triangle")
