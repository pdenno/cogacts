(ns cogacts.model
  (:require [clojure.spec.alpha :as s]))

;;; TODO: What is an "association" in infer and count?

(defrecord Physical [object-name])

(defrecord Sensation [physical        ;; The Physical where sensing is focused. 
                    sense])           ;; The "sensor" used to make the sensation. 

(defrecord Perception [sensations     ;; Sensations, indicating locus of focus (;^) on which 
                       ])

(defrecord MentalObject [perceptions ;; Perceptions on which recognition occurs. 
                          ])

(defrecord Flow [aggregate]) ;; The aggregate (sensations, perception, mental-object) that is flowing.

;;; POD ToDo: ::perceptions

(s/def ::object-name   (s/or :string string? :key keyword?))
(s/def ::physical      (s/and (s/keys :req-un [::object-name])       #(instance? Physical      %)))
(s/def ::sensation     (s/and (s/keys :req-un [::physical ::sense])  #(instance? Sensation     %)))
(s/def ::sensations    (s/or :one ::sensation :multi (s/coll-of ::sensation :kind vector?)))
(s/def ::perception    (s/and (s/keys :req-un [::sensations])        #(instance? Perception    %)))
(s/def ::mental-object (s/and (s/keys :req-un [::perceptions])       #(instance? MentalObject  %)))
(s/def ::flow (s/or :sensation ::sensation :perception ::perception :mental-object ::mental-object))


(defmacro follow-process
  "Execute the process following the form of clojure as->."
  [init var & body]
  `(as-> ~init ~var ~@body))

(defn attend
  "Reify persistent circumstances that are producing sensation, perception or mental object."
  [aggregate]
  (s/assert ::flow sensation)
  aggregate)


;;; TEMPORAL
;;;  attend           -- reify persistent circumstances that are producing sensation 
;;;  poll             -- attend at intervals 
;;;  follow-process   -- perform acts in sequence 
;;;  discover-process -- formulate, take the form of follow-process but requires planning 

;;; MEMORY-RELATED
;;;  recall        -- bringing into a cognitive process (Perhaps for holding, then recognizing) 
;;;  hold          -- attending in short-term memory 
;;;  forget        -- losing the empheral medium. 
;;;  retain        -- the work needed to make a model/image recallable from some sort of record functioning as long term memory.

;;; TRANSITIONING
;;;  perceive      -- form perception (witness phenomenon) from on-going sensing
;;;  form-image    -- Unlike "forming a model" can't write it down without becoming less useful.
;;;  form-model    -- Like "forming and image" but something is gained (not lost!) in writing it down. (A type of attending?)
;;;  recognize     -- form mental object from perception

;;; COGNITIVE
;;;  isolate       -- obtain one from a group (picking instances from all :recognized) 
;;;  infer         -- create association through deductive, abductive, or inductive act 
;;;  count         -- create association to natural numbers
;;;  compare       -- judge one in relation to another.
;;;  abstract      -- MAYBE NOT NEEDED (use recognize)
;;;  partition     -- distinguising according to some model? (Higher order process). 

;;; COMMUNICATE
;;;  mtrans        -- make mental object known to another agent
;;;  translate     -- MAYBE NOT NEEDED

;;; USE MEDIUM
;;;  mark          -- annotate mental object to physical 
;;;  write         -- encode mental object
(defn is-that-waldo?
  "Independent process to Attend, Perceive, Recognize."
  [thing]
  (let [flow (attend (->Sensation :physical thing :sense :visual))]
    (follow-process flow ?f

(defn wheres-waldo?-1 []
  (let [flow (attend {:focus (->Sensation :physical (->Physical :picture)
                               :sense :visual)})]
    (follow-process flow ?f
       (perceive ?f)
       (recognize ?f :figure)              
                  
