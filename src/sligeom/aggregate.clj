(ns sligeom.aggregate
  (:use [slimath core]
        [sligeom core transform bounding intersect])
  (:import [sligeom.transform Transform]
           [sligeom.bounding BBox Bounding]
           [sligeom.intersect Ray]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord Grid3 [^BBox bounds divisions])

(defn divisions-for 
  "compute a heuristically optimal divisions for a bounding box for nprims primitives"
  [^long nprims ^BBox bounds]
  (let [longest-nvoxels (* 3.0 (Math/pow nprims (/ 3.0)))
        nvoxels-per-unit (/ longest-nvoxels (longest bounds))]
    [(* nvoxels-per-unit (.width bounds))
     (* nvoxels-per-unit (.height bounds))
     (* nvoxels-per-unit (.depth bounds))]))

(defn clamp-divisions [[dx dy dz :as d]]
  [(int (clamp dx 1 64)) 
   (int (clamp dy 1 64))
   (int (clamp dz 1 64))])

(defn grid3 [^BBox bounds ^long nprims]
  (->Grid3 bounds (-> nprims
                      (divisions-for bounds)
                      clamp-divisions)))

(defn voxel-size [^Grid3 g]
  (v3div (bbox-size (:bounds g)) (:divisions g)))

(defn point-to-voxel [^Grid3 g 
                        [^double px ^double py ^double pz :as p]]
  (vec (map int 
            (v3div (v3sub p (:minp (:bounds g)))
                   (voxel-size g)))))

(defn voxel-to-point [^Grid3 g [^long vx ^long vy ^long vz :as v]]
  (v3add (:minp (:bounds g))
         (v3mul v (voxel-size g))))

(defn- clamp-vector
  [[^double x ^double y ^double z]]
     [(if (> (Math/abs x) eps-small) x eps-small)
      (if (> (Math/abs y) eps-small) y eps-small)
      (if (> (Math/abs z) eps-small) z eps-small)
      0.0])
 
(defn- sign-vector [[^double x ^double y ^double z]] 
  [(if (pos? x) 1 -1)
   (if (pos? y) 1 -1)
   (if (pos? z) 1 -1)])
     
(defn- entry-point [^Grid3 g ^Ray r]
  (if (contains-point? (:bounds g) (:origin r)) 
    0.0
    (intersect (:bounds g) r)))

(defn grid3-seq [^Grid3 g ^Ray r]
  (if-let [enter-t (entry-point g r)]
    (let [o (ray-at r enter-t)
          d (clamp-vector (:direction r))
          [nx ny nz] (:divisions g)
          [sx sy sz :as s] (sign-vector (:direction r))
          v0 (v3min (point-to-voxel g o)
                    (v3sub (:divisions g) [1 1 1]))
          v1 (v3add v0 (v3max s [0 0 0]))
          tmax (v3abs (v3div (v3sub (voxel-to-point g v1) o) d))
          [tdx tdy tdz :as td] (v3abs (v3div (voxel-size g) d))
          
          step-fx (fn step [[^long x ^long y ^long z] 
                            [^double tmx ^double tmy ^double tmz]]
                    (if (and (< x nx) (>= x 0)
                             (< y ny) (>= y 0)
                             (< z nz) (>= z 0)) 
                      (cons [x y z]
                            (lazy-seq
                             (if (< tmx tmy)
                               (if (< tmx tmz)                                 
                                 (step [(+ x sx) y z]
                                       [(+ tmx tdx) tmy tmz])
                                 (step [x y (+ z sz)]
                                       [tmx tmy (+ tmz tdz)]))
                               (if (< tmy tmz)
                                 (step [x (+ y sy) z]
                                       [tmx (+ tmy tdy) tmz])
                                 (step [x y (+ z sz)]
                                       [tmx tmy (+ tmz tdz)])))))))]
  (step-fx v0 tmax))))
