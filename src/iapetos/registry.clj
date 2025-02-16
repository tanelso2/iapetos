(ns iapetos.registry
  (:refer-clojure :exclude [get name])
  (:require [iapetos.registry
             [collectors :as collectors]
             [utils :as utils]]
            [iapetos.operations :as ops])
  (:import [io.prometheus.client Collector CollectorRegistry]))

;; ## Protocol

(defprotocol Registry
  "Protocol for the iapetos collector registry."
  (subsystem [registry subsystem-name]
    "Create a new registry that is bound to the given subsystem.")
  (register [registry metric collector]
    "Add the given `iapetos.collector/Collector` to the registry using the
     given name.")
  (register-lazy [registry metric collector]
    "Add the given `iapetos.collector/Collector` to the registry using the
     given name, but only actually register it on first use.")
  (unregister [registry metric]
    "Unregister the collector under the given name from the registry.")
  (clear [registry]
    "Clear the registry, removing all collectors from it.")
  (has? [registry metric labels]
    "Returns a boolean if the registry has a given metric configured with the given labels")
  (get [registry metric labels]
    "Retrieve the collector instance associated with the given metric,
     setting the given labels.")
  (raw [registry]
    "Retrieve the underlying `CollectorRegistry`.")
  (name [registry]
    "Retrieve the registry name (for exporting)."))

;; ## Implementation

(declare set-collectors)

(deftype IapetosRegistry [registry-name registry options collectors]
  Registry
  (register [this metric collector]
    (->> (collectors/prepare registry metric collector options)
         (collectors/register collectors)
         (set-collectors this)))
  (register-lazy [this metric collector]
    (->> (collectors/prepare registry metric collector options)
         (collectors/insert collectors)
         (set-collectors this)))
  (unregister [this metric]
    (->> (collectors/lookup collectors metric options)
         (collectors/unregister collectors)
         (set-collectors this)))
  (clear [this]
    (->> (collectors/clear collectors)
         (set-collectors this)))
  (subsystem [_ subsystem-name]
    (assert (string? subsystem-name))
    (IapetosRegistry.
      registry-name
      registry
      (update options :subsystem utils/join-subsystem subsystem-name)
      (collectors/initialize)))
  (has? [_ metric labels]
    (collectors/has? collectors metric labels options))
  (get [_ metric labels]
    (collectors/by collectors metric labels options))
  (raw [_]
    registry)
  (name [_]
    registry-name)

  clojure.lang.IFn
  (invoke [this k]
    (get this k {}))
  (invoke [this k labels]
    (get this k labels))

  clojure.lang.ILookup
  (valAt [this k]
    (get this k {}))
  (valAt [this k default]
    (or (get this k {}) default)))

(defn- set-collectors
  [^IapetosRegistry r collectors]
  (->IapetosRegistry
    (.-registry-name r)
    (.-registry r)
    (.-options r)
    collectors))

(declare wrap-registry)

(deftype StableRegistryRef [^clojure.lang.Atom a]
  Registry
    (register [this metric collector]
      (swap! a register metric collector)
      this)
    (register-lazy [this metric collector]
      (swap! a register-lazy metric collector)
      this)
    (unregister [this metric]
      (swap! a unregister metric)
      this)
    (clear [this]
      (swap! a (fn [^IapetosRegistry x] 
                 (println (str "Running clear on " (raw x) 
                               "\n collectors = " (->> x
                                                       .-collectors 
                                                       (keys))))
                 (clear x)))
      this)
    (subsystem [_ subsystem-name]
      (wrap-registry
        (subsystem @a subsystem-name)))
    (has? [_ metric labels]
      (has? @a metric labels))
    (get [this metric labels]
      (if (has? this metric labels)
        (ops/->LazyCollector (delay (get @a metric labels)))
        nil))
    (raw [_]
      (raw @a))
    (name [_]
      (name @a))
  clojure.lang.IFn
    (invoke [this k]
      (get this k {}))
    (invoke [this k labels]
      (get this k labels))
  clojure.lang.ILookup
    (valAt [this k]
      (get this k {}))
    (valAt [this k default]
      (or (get this k {})
          default))
  clojure.lang.IDeref
    (deref [_] @a))

(defn wrap-registry
  [r]
  (-> r
      atom
      ->StableRegistryRef))
    
;; ## Constructor

(defn create
  ([] (create "iapetos_registry"))
  ([registry-name]
   (create registry-name (CollectorRegistry.)))
  ([registry-name ^CollectorRegistry registry]
   (->> (collectors/initialize)
        (IapetosRegistry. registry-name registry {}))))

(defn create-stable
  [& args]
  (let [r (apply create args)]
    (wrap-registry r)))

(def default
  (create-stable
    "prometheus_default_registry"
    (CollectorRegistry/defaultRegistry)))
