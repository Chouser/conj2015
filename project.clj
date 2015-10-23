(defproject conj2015/exceptions "0.0.1"
  :description "Notes for a talk on exceptions,
  https://docs.google.com/document/d/1-OMeZ5f3BoVv_I4oHrUjn5H9myewb1_ldWhaOIY8sj4/edit

  Examples from PCL, Peter Seibel:
  http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html"
  :dependencies [[org.clojure/clojure "1.7.0"]

                 ^{:voom {:repo "https://github.com/Chouser/error-kit.git"}}
                 [net.n01se/error-kit "1.3.0-20151021_030247-g9029a7b"]

                 [slingshot "0.7.2"]
                 [swell "0.1.0"] ;; meant for slingshot 0.5.0, works up through 0.7.2 (Oct 2011)

                 [bwo/conditions "0.1.0"]
                 [im.chit/hara.event "2.2.11"]])
