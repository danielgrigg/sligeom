(in-ns 'sligeom.core)

(defn intersect-sphere-ray [^double radius ^Ray r]
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

(defn sphere [^double r] (Sphere. r))


