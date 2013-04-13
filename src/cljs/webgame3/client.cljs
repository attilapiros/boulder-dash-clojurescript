(ns hello-clojurescript
  (:require [goog.Timer :as timer]))

;(.log js/console "Hello, world!")

(def size 32)

(def canvas (.getElementById js/document "myCanvas"))

(defn surface [] 
  [
   (. canvas getContext "2d") 
   (. canvas -width) 
   (. canvas -height)
   ]
  )

(defn init-game-state [[_ width height]] {:gamemap gamemap :whereTo :stand :collectedDiamonds 0 :reqiredDiamonds 2})

(def mapsize 14)

(def gamemap  
  (into {} (concat  
             (for [x (range 2 mapsize) y (range 2 mapsize)] [{:x x :y y} :tile])
             (for [x (range 1 mapsize)] [{:x x :y 1} :emptySpace])
             (for [y (range 0 mapsize)] [{:x 1 :y y} :emptySpace])
             (for [y (range 0 mapsize)] [{:x 0 :y y} :concrete])
             (for [x (range 1 mapsize)] [{:x x :y 0} :concrete])
             (for [y (range 0 mapsize)] [{:x mapsize :y y} :concrete]) 
             (for [x (range 0 (+ 1 mapsize))] [{:x x :y mapsize} :concrete])
             (for [x (range 6 mapsize) y (range 6 mapsize)] [{:x x :y y} :emptySpace])
             [[{:x 2 :y 3} :player] [{:x 1 :y 1} :rock]
              [{:x 5 :y 7} :diamond] [{:x 3 :y 8} :diamond]
              [{:x 3 :y 1} :rock] 
              [{:x 5 :y 7} :enemyToRight]
              [{:x 5 :y 9} :enemyToRight] 
              ]
             )
        ))

; (js/alert (:a (js->clj (JSON/parse "{\"a\": 1}") :keywordize-keys true ))) 

(defn initImage [path]
  (let [image (js/Image.)] 
    (do 
      (set! (.-onload image))
      (set! (.-src image) path)
      image
      ))
  )

(def images (let [
                  rock (initImage "images/rock_1.png") 
                  diamond (initImage "images/diamond_1.png")
                  enemy (initImage "images/enemy_down_1.png")]
              {
               :tile (initImage "images/background.png") 
               :concrete (initImage "images/concrete.png") 
               :player (initImage "images/player_standing.png") 
               :rock rock 
               :fallingRock rock 
               :diamond diamond 
               :fallingDiamond diamond
               :emptySpace (initImage "images/empty1.png") 
               :enemyToLeft enemy 
               :enemyToRight enemy 
               :enemyToDown enemy 
               :enemyToUp enemy 
               }))

(defn getImage [actorType] (actorType images))


(def fallingTransformation {
   :rock :fallingRock
   :diamond :fallingDiamond
   :fallingRock :rock
   :fallingDiamond :diamond})

(def directions 
  {
   :right-up {:x 1 :y -1}
   :right-down {:x 1 :y 1}
   :right {:x 1 :y 0}

   :left-up {:x -1 :y -1}
   :left-down {:x -1 :y 1}
   :left {:x -1 :y 0}
   
   :up {:x 0 :y -1}
   :down {:x 0 :y 1}
   })

(def allAroundDelta (for [x '(-1 0 1) y '(-1 0 1) :when (not (and (= x 0) (= 0 y)))] {:x x :y y}))

(defn applyDeltaPos [pos delta] (merge-with + pos delta))

(defn computePos [direction pos] (applyDeltaPos pos (direction directions)))

(def addButton (.getElementById js/document "toAdd"))

(.addEventListener addButton "click" addRectangle)

(def removeButton (.getElementById js/document "toRemove"))

(defn move [state oldPos newPos newType]
  (-> state 
    (assoc-in [:gamemap oldPos] :emptySpace) 
    (assoc-in [:gamemap newPos] newType)))

(defn getFromGamemap [gamemap pos] (get gamemap pos :concrete))

(defn changeToIfNotConcrete [gamemap pos newType] 
  (if-not (= :concrete (getFromGamemap gamemap pos))
    (assoc gamemap pos newType)
    gamemap
  ))

(defn dead [state pos newType] 
  (let [gamemap (:gamemap state)] 
  (assoc state :gamemap 
    (reduce #(assoc %1 %2 newType) 
      (assoc gamemap pos newType)  
      (map #(applyDeltaPos pos %1) 
           allAroundDelta)))))

(defn isEmptySpace [gamemap pos direction]
  (let [newPos (computePos direction pos)] (case (getFromGamemap gamemap newPos) :emptySpace true false)))

(defn mtry [oldPos direction state actorType]
  (let [gamemap (:gamemap state) newPos (computePos direction oldPos) collideType (getFromGamemap gamemap newPos)] 
 (case actorType
  :player 
   (case collideType 
    :tile (move state oldPos newPos :player)
    :emptySpace (move state oldPos newPos :player)
    :diamond (let [currNum (:collectedDiamonds state)] (-> state
               (move oldPos newPos :player)
               (assoc :collectedDiamonds (inc currNum))
               ))
    :rock (let [afterRockPos (computePos direction newPos) 
                isAfterRockEmpty (= :emptySpace (getFromGamemap gamemap afterRockPos))] 
            (if isAfterRockEmpty 
              (-> state 
                (move newPos afterRockPos :rock) 
                (move oldPos newPos :player))
              state))
    state
   )
  (:fallingRock :fallingDiamond)
   (case collideType 
    :emptySpace (move state oldPos newPos actorType)
    (:rock :diamond :tile :concrete) (assoc-in state [:gamemap oldPos] (actorType fallingTransformation))
    :player (dead state newPos :diamond)
    (:enemyToLeft :enemyToRight :enemyToDown :enemyToUp) (dead state newPos :diamond)
    state
   )
  (:enemyToLeft :enemyToDown :enemyToRight :enemyToUp)
   (case collideType
     :emptySpace (move state oldPos newPos actorType)
     state
     ) 
  (:rock :diamond)
   (case collideType 
    :emptySpace (assoc-in state [:gamemap oldPos] (actorType fallingTransformation))
    state
   )
  )
  )
  )

(defn getByTypeSet [gamemap typeSet]
  (filter #(let [[pos actorType] %1] (contains? typeSet actorType)) gamemap))

(defn getByType [gamemap typeToget]
  (first (filter #(let [[pos actorType] %1] (= actorType typeToget)) gamemap)))

(defn checkNoEnemyInNear [gamemap playerPos]
  (every? false? (map #(contains? #{:enemyToLeft :enemyToRight :enemyToUp :enemyToDown} (getFromGamemap gamemap %1)) (map #(computePos %1 playerPos) [:left :right :up :down]))))

(defn playerMove [state] 
  (let [gamemap (:gamemap state) 
        playerPos (first (getByType gamemap :player))
        whereTo (:whereTo state)]
     (if (checkNoEnemyInNear gamemap playerPos)
      (if (not (= :stand whereTo))
         (mtry playerPos whereTo state :player)
        state
        )
       (dead state playerPos :diamond)
       )
  )
)

(defn getRocksAndDiamonds [gamemap] (getByTypeSet gamemap #{:rock :diamond} ))

(defn getFallingRocksAndDiamonds [gamemap] (getByTypeSet gamemap #{:fallingRock :fallingDiamond}))

(defn isBesideAndItsUnderEmpty [state direction pos] 
  (let [gamemap (:gamemap state) 
        beside (computePos direction pos)]
    (if (= :emptySpace (getFromGamemap gamemap beside)) 
        (let [down (computePos :down pos)]
          (if (contains? #{:rock :diamond} (getFromGamemap gamemap down)) 
          (let [besideDown (computePos :down beside)]
            (= :emptySpace (getFromGamemap gamemap besideDown)))
            false)) 
      false)))

(defn isBelowEmpty [state pos]
  (= :emptySpace (get (:gamemap state) (computePos :down pos))))

(defn handleRockInAir [state]
  (reduce #(let [[pos actorType] %2] 
             (if (isBelowEmpty %1 pos) 
              (assoc-in state [:gamemap pos] (actorType fallingTransformation))
              %1))
          state
         (sort-by #(:y (first %)) #(compare %2 %1) (getRocksAndDiamonds (:gamemap state)))))

(defn handleRollers [state]
  (reduce #(let [[pos actorType] %2] 
             (if (isBelowEmpty %1 pos) 
               (mtry pos :down %1 actorType)
               (if (isBesideAndItsUnderEmpty %1 :left pos) 
                 (move %1 pos (computePos :left pos) actorType)
                 (if (isBesideAndItsUnderEmpty %1 :right pos) 
                  (move %1 pos (computePos :right pos) actorType)
                   %1)))
          )
          state 
          (sort-by #(:y (first %)) #(compare %2 %1) (getRocksAndDiamonds (:gamemap state)))))


(defn rockMove [state]
  (-> state 
    handleRockInAir 
    handleRollers))

(defn fallingRockMove [state] 
  (reduce #(let [[pos actorType] %2] 
             (mtry pos :down %1 actorType))
          state 
          (sort-by #(:y (first %)) #(compare %2 %1) (getFallingRocksAndDiamonds (:gamemap state)))))

(def enemyToDirections 
  {:enemyToRight :right 
   :enemyToDown :down 
   :enemyToLeft :left 
   :enemyToUp :up})

(def directionsToEnemey
  {:right  :enemyToRight
   :down :enemyToDown 
   :left :enemyToLeft 
   :up :enemyToUp})

(def directionInClockOrder [:right :down :left :up])
(def duplicatedClockOrder (concat directionInClockOrder directionInClockOrder))
(def preferedDirections 
  {:enemyToDown (take 4 duplicatedClockOrder)
   :enemyToLeft (take 4 (drop 1 duplicatedClockOrder))
   :enemyToUp (take 4 (drop 2 duplicatedClockOrder))
   :enemyToRight (take 4 (drop 3 duplicatedClockOrder))
   }
)

(defn possibleDirection [gamemap pos actorType]
  (filter #(isEmptySpace gamemap pos %1) (actorType preferedDirections)
          ))

(defn getEnemies [gamemap] 
  (getByTypeSet gamemap #{:enemyToLeft :enemyToRight :enemyToDown :enemyToUp}))


(defn enemyMove [state] 
  (reduce #(let [[pos actorType] %2 
                  gamemap (:gamemap %1)]
               ;  (set! (.-title js/document) (str pos originalDirection (isEmptySpace gamemap pos originalDirection)))
                 (let [possibleDirection (possibleDirection gamemap pos actorType)]
                   (if (empty? possibleDirection) 
                     %1 
                    (mtry pos (first possibleDirection) %1 ((first possibleDirection) directionsToEnemey)))))
                 

          state 
          (sort-by #(:y (first %)) #(compare %2 %1) (getEnemies (:gamemap state)))))

(defn autoMove [stateAtom] 
  (swap! stateAtom #(-> %1
             rockMove
             fallingRockMove
             enemyMove
;             playerMove
                      )))

(defn handleKeyUp [stateAtom keyEvent] 
  (swap! stateAtom #(assoc %1 :whereTo :stand)))

(defn handleKeyDown [stateAtom keyEvent] 
  (swap! stateAtom #(assoc %1 :whereTo
    (case (.-keyCode keyEvent)
      37 :left
      38 :up
      39 :right
      40 :down
      :stand))
                      ))
  
(defn playerMoveWrapping [stateAtom] 
  (swap! stateAtom #(
             playerMove %1
                      )))


(defn displayGameElement [pos element context2d]
  (.drawImage 
    context2d
    (getImage element) 
    (* size (:x pos)) 
    (* size (:y pos))))

(defn update-canvas [state surface] 
  (let [[context2d width height] surface gamemap (:gamemap state)]
    (do (set! (.-textContent addButton) (str (:reqiredDiamonds state) " / " (:collectedDiamonds state)))
      (doseq [[key value] (seq gamemap)] 
        (displayGameElement key value context2d)))))

;(.addEventListener removeButton "click" myinit)

;(.addEventListener canvas "keydown" handleKeyDown)


(defn ^:export init []
  (let [surface (surface)
        timer2 (goog.Timer. (/ 1000 30))
        timer1 (goog.Timer. (/ 1000 15))
        timer (goog.Timer. (/ 1000 10))
        stateAtom (atom (init-game-state surface))]

    (goog.events/listen timer2  goog.Timer/TICK #(update-canvas @stateAtom surface))
    (goog.events/listen timer1  goog.Timer/TICK #(do 
                                                  (playerMoveWrapping stateAtom)
                                                  ;(update-canvas @stateAtom surface)
                                                  ))
    (goog.events/listen timer  goog.Timer/TICK #(do 
                                                  (autoMove stateAtom)
                                                  ;(update-canvas @stateAtom surface)
                                                  ))
    (. canvas addEventListener "keyup" #(do 
                                          (handleKeyUp stateAtom %) 
                                          ))
    (. canvas addEventListener "keydown" #(do 
                                            (handleKeyDown stateAtom %) 
                                            ))
    (update-canvas @stateAtom surface)
    (. timer (start))
    (. timer1 (start))
    (. timer2 (start))
    ))
