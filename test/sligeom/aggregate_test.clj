(ns sligeom.aggregate-test
  (:use midje.sweet
        [sligeom core bounding intersect aggregate]))

(fact "`divisions-for` computes an empirical set of divisions"
      (divisions-for 8 (bbox (point3 2 3 4) (point3 4 6 8)))
      => [3.0 4.5 6.0])

(fact "`clamp-divisions` divisons are clamped to a reasonable range"
      (clamp-divisions [-6 23 111])
      => [1 23 64])

(fact "`voxel-size` voxel size given divisions"
      (voxel-size (->Grid3 (bbox (point3 2 3 5) (point3 7 13 9))
                  [2 5 4]))
      => [2.5 2.0 1.0])

(fact "`point-to-voxel` for a point"
      (point-to-voxel (->Grid3 (bbox (point3 2 3 5) (point3 7 13 9)) 
                        [5 5 5])
                        [2 3 5]) 
      => [0 0 0]
                  
      (point-to-voxel (->Grid3 (bbox (point3 2 3 5) (point3 7 13 9))
                        [2 5 4])
                        [3 8 9])
      => [0 2 4])

(fact "`voxel-to-point`"
      (voxel-to-point (->Grid3 (bbox (point3 2 3 5) (point3 7 13 9)) 
                        [5 5 5 1])
                        [0 0 0])
      => [2.0 3.0 5.0]
                  
      (voxel-to-point (->Grid3 (bbox (point3 2 3 5) (point3 7 13 9))
                        [2 5 4])
                        [0 2 4])
      => [2.0 7.0 9.0])

(fact "`grid-seq3` produces a grid-walk seq"
      (grid3-seq (grid3 (bbox (point3 0 0 0) (point3 12 12 12)) 8)
                 (ray (point3 0 0 1) (vector3 2 1.5 0))) 
      => [[0 0 0] [1 0 0] [1 1 0] [2 1 0] [2 2 0] 
          [3 2 0] [3 3 0] [4 3 0] [5 3 0] [5 4 0]])
