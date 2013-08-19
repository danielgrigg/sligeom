(ns sligeom.aggregate-test
  (:use midje.sweet
        [sligeom core bounding intersect aggregate]))

(defn test-grid [minp maxp d]
  (->Grid (bbox minp maxp) d nil))

(fact "`divisions-for` computes an empirical set of divisions"
      (divisions-for 8 (bbox (point3 2 3 4) (point3 4 6 8)))
      => [3.0 4.5 6.0])

(fact "`clamp-divisions` divisons are clamped to a reasonable range"
      (clamp-divisions [-6 23 111])
      => [1 23 64])

(fact "`voxel-size` voxel size given divisions"
      (voxel-size (test-grid (point3 2 3 5) (point3 7 13 9) [2 5 4]))
      => [2.5 2.0 1.0])

(fact "`point-to-voxel` for a point"
      (point-to-voxel (point3 2 3 5)
                      (test-grid (point3 2 3 5) (point3 7 13 9) [5 5 5])) 
      => (point3 0 0 0)
                  
      (point-to-voxel (point3 3 8 9) 
                      (test-grid (point3 2 3 5) (point3 7 13 9) [2 5 4]))
      => [0.4 2.5 4.0 1.0])

(fact "`voxel-to-point`"
      (voxel-to-point (point3 0 0 0)
                      (test-grid (point3 2 3 5) (point3 7 13 9) [5 5 5 1]))
      => (point3 2.0 3.0 5.0)
                  
      (voxel-to-point (point3 0 2 4)
                      (test-grid (point3 2 3 5) (point3 7 13 9) [2 5 4]))
      => (point3 2.0 7.0 9.0))

(fact "`grid-seq` produces a grid-walk seq"
      (grid-seq (grid (bbox (point3 0 0 0) (point3 12 12 12)) 8)
                 (ray (point3 0 0 1) (vector3 2 1.5 0))) 
      => [[0 0 0] [1 0 0] [1 1 0] [2 1 0] [2 2 0] 
          [3 2 0] [3 3 0] [4 3 0] [5 3 0] [5 4 0]]
      (grid-seq (grid (bbox (point3 0 0 0) (point3 12 12 12)) 8)
                 (ray (point3 0 20 4) (vector3 0 0 1)))
      => nil)
      
(fact "`voxel-bbox` computes spanned voxels"
      (voxel-bbox (bbox (point3 0 0 0) (point3 0.2 0.2 0.2)) (grid (bbox (point3 -2 -2 -1) (point3 2 2 1)) 2))
      => (bbox [1 1 0 1] [2 2 1 1]))
