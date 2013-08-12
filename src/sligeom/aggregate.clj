(ns sligeom.aggregate
  (:use [slimath core vec matrix]
        [sligeom core transform bounding intersect])
  (:import [sligeom.transform Transform]
           [sligeom.bounding BBox]
           [sligeom.intersect Ray]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord Grid3 [^BBox bounds divisions voxels])

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
                      clamp-divisions)
           nil))

(defn voxel-size 
  "cartesian size of a voxel in grid g"
  [^Grid3 g]
  (v3div (bbox-size (:bounds g)) (:divisions g)))

(defn point-to-voxel 
  "get xyz voxel index corresponding to a point"
[^Grid3 g [^double px ^double py ^double pz :as p]]
(-> p
    (v3sub (:minp (:bounds g)))
    (v3div (voxel-size g))
    v3int))

(defn voxel-to-point 
  "cartesian point corresponding to voxel index xyz"
  [^Grid3 grid [^long vx ^long vy ^long vz _ :as v]]
  (v3add (:minp (:bounds grid))
         (v3mul v (voxel-size grid))))

;;(defn voxel-index-from-point [grid-bounds divisions p]
;(vec (map int (vdiv3 (vsub3 p (.minp grid-bounds))
;                    (voxel-size grid-bounds divisions)
;))))

(defn- voxel-idx [^Grid3 grid p]
  (v3sub p ))

(defn bbox-voxels "Compute the voxels spanned" [^Grid3 grid ^BBox bounds]
  (bbox (conj (point-to-voxel grid (:minp bounds)) 1)
        (conj (point-to-voxel grid (:maxp bounds)) 1)))

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
  "compute where r enters the grid g"
  (if (contains-point? (:bounds g) (:origin r)) 
    0.0
    (intersect (:bounds g) r)))

(defn grid3-seq
  "sequence the cells intersecting the ray r"
  [^Grid3 g ^Ray r]
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
