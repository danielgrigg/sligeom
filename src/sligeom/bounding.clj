(ns sligeom.bounding
  (:use slimath.core)
  (:use (sligeom core)))

(defprotocol Bounded
  (bounding-box [this] "Compute the AABB"))

(definterface Bounding
  (^double width [])
  (^double height [])
  (^double depth []))

; "An axis-aligned bounding box"
(defrecord BBox [minp maxp]
  Bounding
  (width [this] (- (maxp 0) (minp 0)))
  (height [ this] (- (maxp 1) (minp 1)))
  (depth [this] (- (maxp 2) (minp 2)))

  Bounded
  (bounding-box [this]
    this))

(defn bbox "Construct a BBox"
  ([] 
     (BBox. (point3 (- infinity) (- infinity) (- infinity))
            (point3 infinity infinity infinity)))
  
 ([[x0 y0 z0 w0 :as p0] [x1 y1 z1 w1 :as p1]]
     (BBox. (v4min p0 p1) (v4max p0 p1))))

(defn bbox-centre [^BBox b]
  (v4mul (v4add (.minp b) (.maxp b)) [0.5 0.5 0.5 1.] ))

(defn bbox-size [^BBox b] (vector3 (.width b) (.height b) (.depth b)))

(defn bbox-union [^BBox b p]
  (BBox. (v4min (.minp b) p) (v4max (.maxp b) p)))
