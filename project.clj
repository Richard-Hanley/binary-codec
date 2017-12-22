(defproject binary-codec "0.2.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :deploy-repositories [["releases"  {:sign-releases false :url "https://clojars.org"}]
                        ["snapshots" {:sign-releases false :url "https://clojars.org"}]])
