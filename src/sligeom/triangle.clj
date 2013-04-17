(in-ns 'sligeom.core)

(defn intersect-triangle-ray [p0 p1 p2 ^Ray r]
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

(deftype Triangle [p0 p1 p2]
  RayIntersection
  (intersect [this r]
    (if-let [^Ray _r r]
      (intersect-triangle-ray p0 p1 p2 _r)))
  Object
  (toString [this] (str p0 " " p1 " " p2)))

(defn triangle [p0 p1 p2]
  (Triangle. p0 p1 p2))

