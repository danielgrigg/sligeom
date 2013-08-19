(ns sligeom.bounding
  (:use [slimath core vec matrix]
        [sligeom core]))

(defprotocol Bounded
  (bounding-box [this] "Compute the AABB")
  (contains-point? [this [^double px ^double py ^double pz :as p]]))

(defprotocol Bounding
  (width [this])
  (height [this])
  (depth [this]))

; "An axis-aligned bounding box"
(defrecord BBox [minp maxp]
Bounding
 (width [this] (- (maxp 0) (minp 0)))
 (height [ this] (- (maxp 1) (minp 1)))
 (depth [this] (- (maxp 2) (minp 2)))
  Bounded
  (bounding-box [this]
    this)
  (contains-point? [this [^double px ^double py ^double pz :as p]]
      (let [[x0 y0 z0] (:minp this)
            [x1 y1 z1] (:maxp this)]       
        (not (or (< px x0) (< py y0) (< pz z0)
                 (> px x1) (> py y1) (> pz z1))))))

(declare union)
(defn bbox "Construct a BBox"
  ([] 
     (BBox. (point3 (- infinity) (- infinity) (- infinity))
            (point3 infinity infinity infinity)))
  
 ([[x0 y0 z0 w0 :as p0] [x1 y1 z1 w1 :as p1]]
     (BBox. (v4min p0 p1) (v4max p0 p1)))
 ([xs]
    (->> xs (map bounding-box) (reduce union))))

(defn bbox-centre "centre of b" [^BBox b]
  (v4mul (v4add (.minp b) (.maxp b)) [0.5 0.5 0.5 0.5] ))

(defn bbox-size "size of b" [^BBox b] (vector3 (.width b) (.height b) (.depth b)))

(defn union-point "union of bbox with point" [^BBox b [^double x ^double y ^double z :as p]]
  (BBox. (v4min (.minp b) p) (v4max (.maxp b) p)))

(defn union "Union two geometric objects" [^BBox a ^BBox b]
  (-> a
      (union-point (:minp b)) 
      (union-point (:maxp b))))

(defn bbox-clamp "clamp p to bounds" [p ^BBox bounds]
  (-> (v3clamp p (:minp bounds) (:maxp bounds)) point3))

(defn intersection "intersection of two geometric objects" 
  [^BBox a ^BBox b]
  (bbox (bbox-clamp (:minp a) b) (bbox-clamp (:maxp a) b)))

(defn longest [^BBox b]
  (max (.width b) (.height b) (.depth b)))
