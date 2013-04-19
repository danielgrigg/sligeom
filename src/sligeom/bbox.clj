(in-ns 'sligeom.core)

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


(defn intersect-bbox-ray [^BBox B ^Ray r]
  (let [ea (v3sub (.minp B) (.origin r))
        slab-intersect (fn [^long i ^double t-min ^double t-max]
                         (let [f ((.direction r) i)
                               e (ea i)]
                           (if (> (numeric/abs f) eps-small)
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
    (bbox (transform-point (.minp this) T) (transform-point (.maxp this) T))))


