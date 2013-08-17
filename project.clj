(defproject sligeom "0.3.3"
  :description "Geometric functions"
  :url "http://sliplanesoftware.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "LATEST"]
                 [slimath "LATEST"]
                 [org.clojure/math.numeric-tower "LATEST"]]
  :profiles {:dev {:dependencies [[midje "1.5.0"]]
                   :plugins [[lein-midje "3.1.0"]]}
                   })
