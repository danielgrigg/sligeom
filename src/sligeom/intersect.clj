(ns sligeom.intersect
  (:use [slimath core]
        [sligeom core transform bounding]
        [clojure.math.numeric-tower :only [abs]])
  (:import [sligeom.transform Transform]
           [sligeom.bounding BBox]))

(defprotocol RayIntersection
  ( intersect [this r] "Distance to intersection"))

(defrecord Ray [origin direction ^double mint ^double maxt]
  Transformable
  (transform [this T]
    (Ray. (transform-point T (:origin this))
          (transform-vector T (:direction this))
          mint
          maxt)))
            
(defn ray-at 
"Evaluate r(t)"
  [^Ray r ^double t]
  (v4add (:origin r) (v4muls (:direction r) t)))

(defn ray-interval "Confine the ray r to an interval"
  ([^Ray r ^double mint ^double maxt]
     (Ray. (:origin r) (:direction r) mint maxt))
  ([^Ray r ^double maxt]
     (Ray. (:origin r) (:direction r) (:mint r) maxt)))

(defn ^Ray ray
  "Create a ray"
  ([[ox oy oz ow :as origin] [dx dy dz dw :as direction]]
     (Ray. origin direction eps infinity))
  ([[ox oy oz ow :as origin] [dx dy dz dw :as direction] mint maxt]
     (Ray. origin direction mint maxt)))


(defn intersect-bbox-ray [^BBox B ^Ray r]
  (let [ea (v3sub (.minp B) (.origin r))
        slab-intersect (fn [^long i ^double t-min ^double t-max]
                         (let [f ((.direction r) i)
                               e (ea i)]
                           (if (> (abs f) eps-small)
                             (let [e1 (+ e (.width B))
                                   t1' (/ e1 f)
                                   t2' (/ e f)
                                   [t1 t2] (if (> t1' t2') [t2' t1'] [t1' t2'])
                                   t-min' (max t-min t1)
                                   t-max' (min t-max t2)]
                       (if-not (or (> t-min' t-max') (< t-max' 0))
                         [t-min' t-max']))
                             (if-not (or (> (- (+ e (.width B))) 0) (< (- e) 0))
                               [t-min t-max]))))]
      
    (if-let [[tmin-x tmax-x] (slab-intersect 0 (- infinity) infinity)]
      (if-let [[tmin-y tmax-y] (slab-intersect 1 tmin-x tmax-x)]
        (if-let [[tmin tmax] (slab-intersect 2 tmin-y tmax-y)]
          (let [t (if (> tmin 0.0) tmin tmax)]
            (if (> (.maxt r) t (.mint r)) t)))))))


(extend-type BBox  
  RayIntersection
  (intersect [this _r]
    (let [^Ray r _r]
      (intersect-bbox-ray this r)))

  Transformable
  (transform-object [this T]
    (bbox (transform-point T (.minp this)) (transform-point T (.maxp this)))))


(defn intersect-sphere-ray [^double radius ^Ray r]
  "Ray-sphere intersection"
  (let [A (v3dot (.direction r) (.direction r))
        B (* 2.0 (v3dot (.direction r) (.origin r)))
        C (- (v3dot (.origin r) (.origin r)) (* radius radius ))]
    (if-let [t (quadratic A B C)]
      (let [t0 (first t)
            t1 (second t)]
        (if-not (or (> t0 (.maxt r)) (< t1 (.mint r)))
          (if (< t0 (.mint r))
            (if (> (.maxt r) t1) t1)
            t0))))))

(defrecord Sphere [^double radius]
  RayIntersection
  (intersect [this r]
    (intersect-sphere-ray radius r)))

(defn sphere "Create a sphere for intersection tests" [^double r] (Sphere. r))

(defrecord Plane [position normal]
  RayIntersection
  (intersect [this _r]
    (let [^Ray r _r
          rdotn (v3dot (.direction r) normal)]
      (if (> (abs rdotn) eps-small)
        (let [t (/ (v3dot (v3sub position (.origin r)) normal) rdotn)]
          (if (> (.maxt r) t (.mint r)) t)))))

  Bounded
  (bounding-box [this]
    ;; Bounding-box for an infinite plane...
    (bbox)))

(defn plane "Create a plane for intersection tests"
 [position normal] 
 (Plane. position normal))

(defn intersect-triangle-ray 
  "Ray-triangle intersection"
  [p0 p1 p2 ^Ray r]
  (let [e1 (v3sub p1 p0)
        e2 (v3sub p2 p0)
        pvec (cross (.direction r) e2)
        tvec (v3sub (.origin r) p0)
        qvec (cross tvec e1)
        det (v3dot e1 pvec)
        u (v3dot tvec pvec)
        v (v3dot (.direction r) qvec)]
    (if (and (> det eps) (>= u 0.0) (<= u det) (>= v 0.0) (<= (+ u v) det))
      (v3divs [(v3dot e2 qvec) u v] det))))

(defrecord Triangle [p0 p1 p2]
  RayIntersection
  (intersect [this r]
    (if-let [^Ray _r r]
      (intersect-triangle-ray p0 p1 p2 _r))))

(defn triangle 
  "Create a triangle for intersection tests"
  [p0 p1 p2]
  (Triangle. p0 p1 p2))

