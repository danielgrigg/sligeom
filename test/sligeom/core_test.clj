(ns sligeom.core-test
  (:use midje.sweet
        sligeom.core)
  (:require [clojure.string :as str]))

(fact "`point3` creates a 3d point"
      (point3 2 3 5) => [2.0 3.0 5.0 1.0]
      (point3 -3.2 7.8 -99.5) => [-3.2 7.8 -99.5 1.0])