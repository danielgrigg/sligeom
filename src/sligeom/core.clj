(ns sligeom.core
  (:use slimath.core)
  (:use [clojure.pprint :only [pprint]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(load "transform")