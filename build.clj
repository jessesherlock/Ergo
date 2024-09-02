(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.jessesherlock/ergo)

(def version (format "0.1.%s-alpha" (b/git-count-revs nil)))

(def class-dir "target/classes")

(def jar-file (format "target/%s-%s.jar" (name lib) version))

(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :pom-data
                [[:distributionManagement
                  [:repository
                   [:id "clojars"]
                   [:name "Clojars Repository"]
                   [:url "https://clojars.org/repo"]]]
                 [:licenses
                  [:license
                   [:name "Eclipse Public License -v 1.0"]
                   [:url "https://www.eclipse.org/org/documents/epl-v10.html"]
                   [:distribution "repo"]]]]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))
